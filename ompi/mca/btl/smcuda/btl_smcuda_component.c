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
 * Copyright (c) 2011-2012 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */

#include "ompi/constants.h"
#include "opal/mca/event/event.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mpool/base/base.h"
#if OMPI_CUDA_SUPPORT
#include "ompi/runtime/params.h"
#include "ompi/mca/common/cuda/common_cuda.h"
#endif /* OMPI_CUDA_SUPPORT */
#include "ompi/mca/common/sm/common_sm.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "btl_smcuda.h"
#include "btl_smcuda_frag.h"
#include "btl_smcuda_fifo.h"

static int mca_btl_smcuda_component_open(void);
static int mca_btl_smcuda_component_close(void);
static int smcuda_register(void);
static mca_btl_base_module_t** mca_btl_smcuda_component_init(
    int *num_btls,
    bool enable_progress_threads,
    bool enable_mpi_threads
);


/*
 * Shared Memory (SM) component instance.
 */
mca_btl_smcuda_component_t mca_btl_smcuda_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "smcuda", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_smcuda_component_open,  /* component open */
            mca_btl_smcuda_component_close,  /* component close */
            NULL,
            smcuda_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_smcuda_component_init,
        mca_btl_smcuda_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_smcuda_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_smcuda_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("btl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


static int smcuda_register(void)
{
    /* register SM component parameters */
    mca_btl_smcuda_component.sm_free_list_num =
        mca_btl_smcuda_param_register_int("free_list_num", 8);
    mca_btl_smcuda_component.sm_free_list_max =
        mca_btl_smcuda_param_register_int("free_list_max", -1);
    mca_btl_smcuda_component.sm_free_list_inc =
        mca_btl_smcuda_param_register_int("free_list_inc", 64);
    mca_btl_smcuda_component.sm_max_procs =
        mca_btl_smcuda_param_register_int("max_procs", -1);
    mca_btl_smcuda_component.sm_mpool_name =
        mca_btl_smcuda_param_register_string("mpool", "sm");
    mca_btl_smcuda_component.fifo_size =
        mca_btl_smcuda_param_register_int("fifo_size", 4096);
    mca_btl_smcuda_component.nfifos =
        mca_btl_smcuda_param_register_int("num_fifos", 1);

    mca_btl_smcuda_component.fifo_lazy_free =
        mca_btl_smcuda_param_register_int("fifo_lazy_free", 120);

    /* default number of extra procs to allow for future growth */
    mca_btl_smcuda_component.sm_extra_procs =
        mca_btl_smcuda_param_register_int("sm_extra_procs", 0);

#if OMPI_CUDA_SUPPORT
    mca_btl_smcuda.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;
#else /* OMPI_CUDA_SUPPORT */
    mca_btl_smcuda.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH-1;
#endif /* OMPI_CUDA_SUPPORT */
    mca_btl_smcuda.super.btl_eager_limit = 4*1024;
    mca_btl_smcuda.super.btl_rndv_eager_limit = 4*1024;
    mca_btl_smcuda.super.btl_max_send_size = 32*1024;
    mca_btl_smcuda.super.btl_rdma_pipeline_send_length = 64*1024;
    mca_btl_smcuda.super.btl_rdma_pipeline_frag_size = 64*1024;
    mca_btl_smcuda.super.btl_min_rdma_pipeline_size = 64*1024;
    mca_btl_smcuda.super.btl_flags = MCA_BTL_FLAGS_SEND;
#if OMPI_CUDA_SUPPORT
    mca_btl_smcuda.super.btl_flags |= MCA_BTL_FLAGS_CUDA_GET;
#endif /* OMPI_CUDA_SUPPORT */
    mca_btl_smcuda.super.btl_bandwidth = 9000;  /* Mbs */
    mca_btl_smcuda.super.btl_latency   = 1;     /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_smcuda_component.super.btl_version,
                                &mca_btl_smcuda.super);

    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_smcuda_component_open(void)
{
    mca_btl_smcuda_component.sm_max_btls = 1;

    /* make sure the number of fifos is a power of 2 */
    mca_btl_smcuda_component.nfifos = opal_next_poweroftwo_inclusive (mca_btl_smcuda_component.nfifos);

    /* make sure that queue size and lazy free parameter are compatible */
    if (mca_btl_smcuda_component.fifo_lazy_free >= (mca_btl_smcuda_component.fifo_size >> 1) )
        mca_btl_smcuda_component.fifo_lazy_free  = (mca_btl_smcuda_component.fifo_size >> 1);
    if (mca_btl_smcuda_component.fifo_lazy_free <= 0)
        mca_btl_smcuda_component.fifo_lazy_free  = 1;

    mca_btl_smcuda_component.max_frag_size = mca_btl_smcuda.super.btl_max_send_size;
    mca_btl_smcuda_component.eager_limit = mca_btl_smcuda.super.btl_eager_limit;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_smcuda_component.sm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_smcuda_component.sm_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_smcuda_component.sm_frags_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_smcuda_component.sm_frags_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_smcuda_component.pending_send_fl, opal_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_smcuda_component_close(void)
{
    int return_value = OMPI_SUCCESS;


    OBJ_DESTRUCT(&mca_btl_smcuda_component.sm_lock);
    /**
     * We don't have to destroy the fragment lists. They are allocated
     * directly into the mmapped file, they will auto-magically disappear
     * when the file get unmapped.
     */
    /*OBJ_DESTRUCT(&mca_btl_smcuda_component.sm_frags_eager);*/
    /*OBJ_DESTRUCT(&mca_btl_smcuda_component.sm_frags_max);*/

    /* unmap the shared memory control structure */
    if(mca_btl_smcuda_component.sm_seg != NULL) {
        return_value = mca_common_sm_fini( mca_btl_smcuda_component.sm_seg );
        if( OMPI_SUCCESS != return_value ) {
            return_value=OMPI_ERROR;
            opal_output(0," mca_common_sm_fini failed\n");
            goto CLEANUP;
        }

        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scenario,
         * this file will still get cleaned up */
#if OPAL_ENABLE_FT_CR    == 1
        /* Only unlink the file if we are *not* restarting
         * If we are restarting the file will be unlinked at a later time.
         */
        if(OPAL_CR_STATUS_RESTART_PRE  != opal_cr_checkpointing_state &&
           OPAL_CR_STATUS_RESTART_POST != opal_cr_checkpointing_state ) {
            unlink(mca_btl_smcuda_component.sm_seg->shmem_ds.seg_name);
        }
#else
        unlink(mca_btl_smcuda_component.sm_seg->shmem_ds.seg_name);
#endif
        OBJ_RELEASE(mca_btl_smcuda_component.sm_seg);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_btl_smcuda_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_btl_smcuda_component.sm_fifo_fd,&cmd,sizeof(cmd)) !=
                sizeof(cmd)){
            opal_output(0, "mca_btl_smcuda_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        opal_thread_join(&mca_btl_smcuda_component.sm_fifo_thread, NULL);
        close(mca_btl_smcuda_component.sm_fifo_fd);
        unlink(mca_btl_smcuda_component.sm_fifo_path);
    }
#endif

    if (NULL != mca_btl_smcuda_component.sm_mpool_name) {
        free(mca_btl_smcuda_component.sm_mpool_name);
    }

CLEANUP:

    /* return */
    return return_value;
}

/*
 *  SM component initialization
 */
static mca_btl_base_module_t** mca_btl_smcuda_component_init(
    int *num_btls,
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_btl_base_module_t **btls = NULL;

    *num_btls = 0;

    /* if no session directory was created, then we cannot be used */
    if (!orte_create_session_dirs) {
        return NULL;
    }
    
    /* lookup/create shared memory pool only when used */
    mca_btl_smcuda_component.sm_mpool = NULL;
    mca_btl_smcuda_component.sm_mpool_base = NULL;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf( mca_btl_smcuda_component.sm_fifo_path,
             "%s"OPAL_PATH_SEP"sm_fifo.%lu", orte_process_info.job_session_dir,
             (unsigned long)ORTE_PROC_MY_NAME->vpid );
    if(mkfifo(mca_btl_smcuda_component.sm_fifo_path, 0660) < 0) {
        opal_output(0, "mca_btl_smcuda_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_btl_smcuda_component.sm_fifo_fd = open(mca_btl_smcuda_component.sm_fifo_path, O_RDWR);
    if(mca_btl_smcuda_component.sm_fifo_fd < 0) {
        opal_output(0, "mca_btl_smcuda_component_init: open(%s) failed with errno=%d\n",
                    mca_btl_smcuda_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_btl_smcuda_component.sm_fifo_thread, opal_thread_t);
    mca_btl_smcuda_component.sm_fifo_thread.t_run = (opal_thread_fn_t) mca_btl_smcuda_component_event_thread;
    opal_thread_start(&mca_btl_smcuda_component.sm_fifo_thread);
#endif

    mca_btl_smcuda_component.sm_btls = (mca_btl_smcuda_t **) malloc( mca_btl_smcuda_component.sm_max_btls * sizeof (mca_btl_smcuda_t *));
    if (NULL == mca_btl_smcuda_component.sm_btls) {
        return NULL;
    }

    /* allocate the Shared Memory BTL */
    *num_btls = 1;
    btls = (mca_btl_base_module_t**)malloc(sizeof(mca_btl_base_module_t*));
    if (NULL == btls) {
        return NULL;
    }

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t*)(&(mca_btl_smcuda));
    mca_btl_smcuda_component.sm_btls[0] = (mca_btl_smcuda_t*)(&(mca_btl_smcuda));

    /* initialize some BTL data */
    /* start with no SM procs */
    mca_btl_smcuda_component.num_smp_procs = 0;
    mca_btl_smcuda_component.my_smp_rank   = -1;  /* not defined */
    mca_btl_smcuda_component.sm_num_btls   = 1;
    /* set flag indicating btl not inited */
    mca_btl_smcuda.btl_inited = false;

#if OMPI_CUDA_SUPPORT
    /* Assume CUDA GET works. */
	mca_btl_smcuda.super.btl_get = mca_btl_smcuda_get_cuda;
#endif /* OMPI_CUDA_SUPPORT */

    return btls;

}


/*
 *  SM component progress.
 */

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void mca_btl_smcuda_component_event_thread(opal_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_btl_smcuda_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        }
        mca_btl_smcuda_component_progress();
    }
}
#endif

void btl_smcuda_process_pending_sends(struct mca_btl_base_endpoint_t *ep) 
{ 
    btl_smcuda_pending_send_item_t *si; 
    int rc; 

    while ( 0 < opal_list_get_size(&ep->pending_sends) ) {
        /* Note that we access the size of ep->pending_sends unlocked
           as it doesn't really matter if the result is wrong as 
           opal_list_remove_first is called with a lock and we handle it
           not finding an item to process */
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        si = (btl_smcuda_pending_send_item_t*)opal_list_remove_first(&ep->pending_sends); 
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);

        if(NULL == si) return; /* Another thread got in before us. Thats ok. */
    
        OPAL_THREAD_ADD32(&mca_btl_smcuda_component.num_pending_sends, -1);

        MCA_BTL_SMCUDA_FIFO_WRITE(ep, ep->my_smp_rank, ep->peer_smp_rank, si->data,
                          true, false, rc);

        OPAL_FREE_LIST_RETURN(&mca_btl_smcuda_component.pending_send_fl, (opal_list_item_t*)si);

        if ( OMPI_SUCCESS != rc )
            return;
    }
} 

int mca_btl_smcuda_component_progress(void)
{
    /* local variables */
    mca_btl_smcuda_frag_t *frag;
    mca_btl_smcuda_frag_t Frag;
    sm_fifo_t *fifo = NULL;
    mca_btl_smcuda_hdr_t *hdr;
    int my_smp_rank = mca_btl_smcuda_component.my_smp_rank;
    int peer_smp_rank, j, rc = 0, nevents = 0;

    /* first, deal with any pending sends */
    /* This check should be fast since we only need to check one variable. */
    if ( 0 < mca_btl_smcuda_component.num_pending_sends ) {

        /* perform a loop to find the endpoints that have pending sends */
        /* This can take a while longer if there are many endpoints to check. */
        for ( peer_smp_rank = 0; peer_smp_rank < mca_btl_smcuda_component.num_smp_procs; peer_smp_rank++) {
            struct mca_btl_base_endpoint_t* endpoint;
            if ( peer_smp_rank == my_smp_rank )
                continue;
            endpoint = mca_btl_smcuda_component.sm_peers[peer_smp_rank];
            if ( 0 < opal_list_get_size(&endpoint->pending_sends) )
                btl_smcuda_process_pending_sends(endpoint);
        }
    }

    /* poll each fifo */
    for(j = 0; j < FIFO_MAP_NUM(mca_btl_smcuda_component.num_smp_procs); j++) {
        fifo = &(mca_btl_smcuda_component.fifo[my_smp_rank][j]);
      recheck_peer:
        /* aquire thread lock */
        if(opal_using_threads()) {
            opal_atomic_lock(&(fifo->tail_lock));
        }

        hdr = (mca_btl_smcuda_hdr_t *)sm_fifo_read(fifo);

        /* release thread lock */
        if(opal_using_threads()) {
            opal_atomic_unlock(&(fifo->tail_lock));
        }

        if(SM_FIFO_FREE == hdr) {
            continue;
        }

        nevents++;
        /* dispatch fragment by type */
        switch(((uintptr_t)hdr) & MCA_BTL_SMCUDA_FRAG_TYPE_MASK) {
            case MCA_BTL_SMCUDA_FRAG_SEND:
            {
                mca_btl_active_message_callback_t* reg;
                /* change the address from address relative to the shared
                 * memory address, to a true virtual address */
                hdr = (mca_btl_smcuda_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
#if OPAL_ENABLE_DEBUG
                if ( FIFO_MAP(peer_smp_rank) != j ) {
                    opal_output(0, "mca_btl_smcuda_component_progress: "
                                "rank %d got %d on FIFO %d, but this sender should send to FIFO %d\n",
                                my_smp_rank, peer_smp_rank, j, FIFO_MAP(peer_smp_rank));
                }
#endif
                /* recv upcall */
                reg = mca_btl_base_active_message_trigger + hdr->tag;
                Frag.segment.seg_addr.pval = ((char*)hdr) +
                    sizeof(mca_btl_smcuda_hdr_t);
                Frag.segment.seg_len = hdr->len;
                Frag.base.des_dst_cnt = 1;
                Frag.base.des_dst = &(Frag.segment);
                reg->cbfunc(&mca_btl_smcuda.super, hdr->tag, &(Frag.base),
                            reg->cbdata);
                /* return the fragment */
                MCA_BTL_SMCUDA_FIFO_WRITE(
                        mca_btl_smcuda_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr->frag, false, true, rc);
                break;
            }
        case MCA_BTL_SMCUDA_FRAG_ACK:
            {
                int status = (uintptr_t)hdr & MCA_BTL_SMCUDA_FRAG_STATUS_MASK;
                int btl_ownership;
                struct mca_btl_base_endpoint_t* endpoint;

                frag = (mca_btl_smcuda_frag_t *)((char*)((uintptr_t)hdr &
                                                     (~(MCA_BTL_SMCUDA_FRAG_TYPE_MASK |
                                                        MCA_BTL_SMCUDA_FRAG_STATUS_MASK))));

                endpoint = frag->endpoint;
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                    /* completion callback */
                    frag->base.des_cbfunc(&mca_btl_smcuda.super, frag->endpoint,
                                          &frag->base, status?OMPI_ERROR:OMPI_SUCCESS);
                }
                if( btl_ownership ) {
                    MCA_BTL_SMCUDA_FRAG_RETURN(frag);
                }
                OPAL_THREAD_ADD32(&mca_btl_smcuda_component.num_outstanding_frags, -1);
                if ( 0 < opal_list_get_size(&endpoint->pending_sends) ) {
                    btl_smcuda_process_pending_sends(endpoint);
                }
                goto recheck_peer;
            }
            default:
                /* unknown */
                /*
                 * This code path should presumably never be called.
                 * It's unclear if it should exist or, if so, how it should be written.
                 * If we want to return it to the sending process,
                 * we have to figure out who the sender is.
                 * It seems we need to subtract the mask bits.
                 * Then, hopefully this is an sm header that has an smp_rank field.
                 * Presumably that means the received header was relative.
                 * Or, maybe this code should just be removed.
                 */
                opal_output(0, "mca_btl_smcuda_component_progress read an unknown type of header");
                hdr = (mca_btl_smcuda_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
                hdr = (mca_btl_smcuda_hdr_t*)((uintptr_t)hdr->frag |
                        MCA_BTL_SMCUDA_FRAG_STATUS_MASK);
                MCA_BTL_SMCUDA_FIFO_WRITE(
                        mca_btl_smcuda_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr, false, true, rc);
                break;
        }
    }

#if OMPI_CUDA_SUPPORT
    /* Check to see if there are any outstanding CUDA events that have
     * completed.  If so, issue the PML callbacks on the fragments.
     */
    while (1 == progress_one_cuda_event((mca_btl_base_descriptor_t **)&frag)) {
        int btl_ownership;
        btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (0 != (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags)) {
            frag->base.des_cbfunc(&mca_btl_smcuda.super, 
                                  frag->endpoint, &frag->base, 
                                  OMPI_SUCCESS);
        }

        if (btl_ownership) {
            if(frag->registration != NULL) {
                frag->endpoint->mpool->mpool_deregister(frag->endpoint->mpool,
                                                       (mca_mpool_base_registration_t*)frag->registration);
                frag->registration = NULL;
            }
            MCA_BTL_SMCUDA_FRAG_RETURN(frag);
        }
        nevents++;
    }
#endif /* OMPI_CUDA_SUPPORT */
    return nevents;
}
