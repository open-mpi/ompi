/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_fifo.h"

static int mca_btl_vader_component_progress (void);
static int mca_btl_vader_component_open(void);
static int mca_btl_vader_component_close(void);
static int mca_btl_vader_component_register(void);
static mca_btl_base_module_t** mca_btl_vader_component_init(int *num_btls,
							    bool enable_progress_threads,
							    bool enable_mpi_threads);

/* limit where we should switch from bcopy to memcpy */
int mca_btl_vader_memcpy_limit     = 524288;
int mca_btl_vader_segment_multiple = 4194304;
/* maximum size for using copy-in-copy out semantics for contiguous sends */
int mca_btl_vader_max_inline_send = 256;

/*
 * Shared Memory (VADER) component instance.
 */
mca_btl_vader_component_t mca_btl_vader_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
	   about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "vader", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_vader_component_open,  /* component open */
            mca_btl_vader_component_close,  /* component close */
            NULL,
            mca_btl_vader_component_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_vader_component_init,
        mca_btl_vader_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char *mca_btl_vader_param_register_string(const char *param_name,
							const char *default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl", "vader",
					    param_name, NULL,
					    default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_vader_param_register_int(const char *param_name,
						   int value)
{
    int id = mca_base_param_register_int("btl", "vader", param_name,
					 NULL, value);
    mca_base_param_lookup_int(id, &value);
    return value;
}

static int msb (int x)
{
    x |= (x >> 1);
    x |= (x >> 2);
    x |= (x >> 4);
    x |= (x >> 8);
    x |= (x >> 16);
    return (x & ~(x >> 1));
}

static int mca_btl_vader_component_register (void)
{
    /* register VADER component parameters */
    mca_btl_vader_component.vader_free_list_num =
        mca_btl_vader_param_register_int("free_list_num", 8);
    mca_btl_vader_component.vader_free_list_max =
        mca_btl_vader_param_register_int("free_list_max", -1);
    mca_btl_vader_component.vader_free_list_inc =
        mca_btl_vader_param_register_int("free_list_inc", 64);
    mca_btl_vader_component.vader_mpool_name =
        mca_btl_vader_param_register_string("mpool", "sm");
    mca_btl_vader_memcpy_limit =
	mca_btl_vader_param_register_int("memcpy_limit", mca_btl_vader_memcpy_limit);
    mca_btl_vader_segment_multiple =
	msb(mca_btl_vader_param_register_int("segment_multiple", mca_btl_vader_segment_multiple));
    mca_btl_vader_max_inline_send =
	mca_btl_vader_param_register_int("max_inline_send", mca_btl_vader_max_inline_send);

    mca_btl_vader.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;
    mca_btl_vader.super.btl_eager_limit = 64 * 1024;
    mca_btl_vader.super.btl_rndv_eager_limit = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_max_send_size    = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_rdma_pipeline_send_length = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_rdma_pipeline_frag_size = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_min_rdma_pipeline_size = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_flags = MCA_BTL_FLAGS_GET | MCA_BTL_FLAGS_PUT |
	MCA_BTL_FLAGS_SEND_INPLACE;

    mca_btl_vader.super.btl_bandwidth = 40000; /* Mbs */
    mca_btl_vader.super.btl_latency   = 1;     /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_vader_component.super.btl_version,
                                &mca_btl_vader.super);

    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_vader_component_open(void)
{
    mca_btl_vader_component.eager_limit = mca_btl_vader.super.btl_eager_limit;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_vader_component.vader_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_vader_component.vader_frags_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_vader_component.active_sends, opal_list_t);

    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_vader_component_close(void)
{
    int return_value = OMPI_SUCCESS;

    /**
     * We don't have to destroy the fragment lists. They are allocated
     * directly into the mmapped file, they will auto-magically disappear
     * when the file get unmapped.
     */
    /*OBJ_DESTRUCT(&mca_btl_vader_component.vader_frags_eager);*/

    /* unmap the shared memory control structure */
    if(mca_btl_vader_component.vader_seg != NULL) {
        return_value = mca_common_sm_fini( mca_btl_vader_component.vader_seg );
        if( OMPI_SUCCESS != return_value ) {
            return_value=OMPI_ERROR;
            opal_output(0," mca_common_sm_fini failed\n");
            goto CLEANUP;
        }

        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scenario,
         * this file will still get cleaned up */
	/* XXX LANL TODO -- remove unlink once the shmem segment uses xpmem */
        unlink(mca_btl_vader_component.vader_seg->shmem_ds.seg_name);
        OBJ_RELEASE(mca_btl_vader_component.vader_seg);
    }

    if (NULL != mca_btl_vader_component.vader_mpool_name) {
        free(mca_btl_vader_component.vader_mpool_name);
    }

    OBJ_DESTRUCT(&mca_btl_vader_component.active_sends);

 CLEANUP:

    /* return */
    return return_value;
}

/*
 *  VADER component initialization
 */
static mca_btl_base_module_t **mca_btl_vader_component_init (int *num_btls,
							     bool enable_progress_threads,
							     bool enable_mpi_threads)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    mca_btl_base_module_t **btls = NULL;

    *num_btls = 0;

    /* if no session directory was created, then we cannot be used */
    /* XXX LANL FIXME -- this is not the case. we can use an anonymous segment */
    if (!orte_create_session_dirs) {
        return NULL;
    }
    
    /* lookup/create shared memory pool only when used */
    component->vader_mpool      = NULL;
    component->vader_mpool_base = NULL;

    btls = (mca_btl_base_module_t **) calloc (1, sizeof (mca_btl_base_module_t *));
    if (NULL == btls) {
        return NULL;
    }

    *num_btls = 1;

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t *) &mca_btl_vader;

    /* initialize some BTL data */
    /* start with no VADER procs */
    component->num_smp_procs    = 0;
    component->my_smp_rank      = -1;  /* not defined */

    /* set flag indicating btl not inited */
    mca_btl_vader.btl_inited = false;

    return btls;
}

static inline void mca_btl_vader_progress_sends (void)
{
    opal_list_t *list = &mca_btl_vader_component.active_sends;
    opal_list_item_t *item, *next;
    mca_btl_vader_frag_t *frag;

    for (item = opal_list_get_first (list) ; item != opal_list_get_end (list) ; ) {
	frag = (mca_btl_vader_frag_t *) item;
	next = opal_list_get_next (item);

	if (OPAL_LIKELY(frag->hdr->complete)) {
	    opal_list_remove_item (&mca_btl_vader_component.active_sends, item);

	    if (OPAL_UNLIKELY(MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags)) {
		/* completion callback */
		frag->base.des_cbfunc(&mca_btl_vader.super, frag->endpoint,
				      &frag->base, OMPI_SUCCESS);
	    }

	    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
		MCA_BTL_VADER_FRAG_RETURN(frag);
	    }
	}

	item = next;
    }
}

static int mca_btl_vader_component_progress (void)
{
    int my_smp_rank = mca_btl_vader_component.my_smp_rank;
    vader_fifo_t *fifo = mca_btl_vader_component.fifo[my_smp_rank];
    mca_btl_active_message_callback_t *reg;
    mca_btl_vader_frag_t frag;
    mca_btl_vader_hdr_t *hdr;
    mca_btl_base_segment_t segments[2];
    mca_mpool_base_registration_t *xpmem_reg = NULL;
    bool single_copy;

    /* check active sends for completion */
    mca_btl_vader_progress_sends ();

    /* poll the fifo once */
    hdr = (mca_btl_vader_hdr_t *) vader_fifo_read (fifo);
    if (VADER_FIFO_FREE == hdr) {
	return 0;
    }

    /* change the address from address relative to the shared
     * memory address, to a true virtual address */
    hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(hdr);

    reg = mca_btl_base_active_message_trigger + hdr->tag;
    frag.base.des_dst     = segments;

    segments[0].seg_addr.pval = (void *) (hdr + 1);
    segments[0].seg_len       = hdr->len;

    if (OPAL_UNLIKELY(hdr->flags & MCA_BTL_VADER_FLAG_SINGLE_COPY)) {
	struct iovec *rem_mem = (struct iovec *) ((uintptr_t)segments[0].seg_addr.pval + hdr->len);

	xpmem_reg = vader_get_registation (hdr->my_smp_rank, rem_mem->iov_base,
					   rem_mem->iov_len, 0);

	segments[1].seg_addr.pval = vader_reg_to_ptr (xpmem_reg, rem_mem->iov_base);
	segments[1].seg_len       = rem_mem->iov_len;

	/* recv upcall */
	frag.base.des_dst_cnt = 2;
	reg->cbfunc(&mca_btl_vader.super, hdr->tag, &(frag.base), reg->cbdata);
	vader_return_registration (xpmem_reg, hdr->my_smp_rank);
    } else {
	frag.base.des_dst_cnt = 1;
	reg->cbfunc(&mca_btl_vader.super, hdr->tag, &(frag.base), reg->cbdata);
    }

    /* return the fragment */
    hdr->complete = true;

    return 1;
}
