/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/util/output.h"
#include "opal/threads/mutex.h"
#include "opal/mca/btl/base/btl_base_error.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_fifo.h"
#include "btl_vader_fbox.h"
#include "btl_vader_xpmem.h"

#include <sys/mman.h>

static int mca_btl_vader_component_progress (void);
static int mca_btl_vader_component_open(void);
static int mca_btl_vader_component_close(void);
static int mca_btl_vader_component_register(void);
static mca_btl_base_module_t** mca_btl_vader_component_init(int *num_btls,
                                                            bool enable_progress_threads,
                                                            bool enable_mpi_threads);

/*
 * Shared Memory (VADER) component instance.
 */
mca_btl_vader_component_t mca_btl_vader_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("vader"),
            .mca_open_component = mca_btl_vader_component_open,
            .mca_close_component = mca_btl_vader_component_close,
            .mca_register_component_params = mca_btl_vader_component_register,
        },
        .btl_data = {
            /* The component is checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .btl_init = mca_btl_vader_component_init,
        .btl_progress = mca_btl_vader_component_progress,
    }  /* end super */
};

static int mca_btl_vader_component_register (void)
{
    (void) mca_base_var_group_component_register(&mca_btl_vader_component.super.btl_version,
                                                 "XPMEM shared memory byte transport later");

    /* register VADER component variables */
    mca_btl_vader_component.vader_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "free_list_num", "Initial number of fragments "
                                           "to allocate for shared memory communication.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.vader_free_list_num);
    mca_btl_vader_component.vader_free_list_max = 16384;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "free_list_max", "Maximum number of fragments "
                                           "to allocate for shared memory communication.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.vader_free_list_max);
    mca_btl_vader_component.vader_free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "free_list_inc", "Number of fragments to create "
                                           "on each allocation.", MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.vader_free_list_inc);

    mca_btl_vader_component.memcpy_limit = 524288;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "memcpy_limit", "Message size to switch from using "
                                           "memove to memcpy. The relative speed of these two "
                                           "routines can vary by size.", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.memcpy_limit);
#if OPAL_BTL_VADER_HAVE_XPMEM
    mca_btl_vader_component.log_attach_align = 21;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "log_align", "Log base 2 of the alignment to use for xpmem "
                                           "segments (default: 21, minimum: 12, maximum: 25)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.log_attach_align);
#endif

#if OPAL_BTL_VADER_HAVE_XPMEM && 64 == MCA_BTL_VADER_BITNESS
    mca_btl_vader_component.segment_size = 1 << 24;
#else
    mca_btl_vader_component.segment_size = 1 << 22;
#endif
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "segment_size", "Maximum size of all shared "
#if OPAL_BTL_VADER_HAVE_XPMEM && 64 == MCA_BTL_VADER_BITNESS
                                           "memory buffers (default: 16M)",
#else
                                           "memory buffers (default: 4M)",
#endif
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.segment_size);

    mca_btl_vader_component.max_inline_send = 256;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "max_inline_send", "Maximum size to transfer "
                                           "using copy-in copy-out semantics",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_vader_component.max_inline_send);

    mca_btl_vader_component.fbox_threshold = 16;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "fbox_threshold", "Number of sends required "
                                           "before an eager send buffer is setup for a peer "
                                           "(default: 16)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL,
                                           0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_vader_component.fbox_threshold);

    mca_btl_vader_component.fbox_max = 32;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "fbox_max", "Maximum number of eager send buffers "
                                           "to allocate (default: 32)", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_vader_component.fbox_max);

    mca_btl_vader_component.fbox_size = 4096;
    (void) mca_base_component_var_register(&mca_btl_vader_component.super.btl_version,
                                           "fbox_size", "Size of per-peer fast transfer buffers (default: 4k)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_vader_component.fbox_size);

    mca_btl_vader.super.btl_exclusivity               = MCA_BTL_EXCLUSIVITY_HIGH;
#if OPAL_BTL_VADER_HAVE_XPMEM
    mca_btl_vader.super.btl_eager_limit               = 32 * 1024;
    mca_btl_vader.super.btl_rndv_eager_limit          = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_max_send_size             = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_min_rdma_pipeline_size    = mca_btl_vader.super.btl_eager_limit;
#else
    mca_btl_vader.super.btl_eager_limit               = 4 * 1024;
    mca_btl_vader.super.btl_rndv_eager_limit          = 32 * 1024;
    mca_btl_vader.super.btl_max_send_size             = 32 * 1024;
    mca_btl_vader.super.btl_min_rdma_pipeline_size    = 32 * 1024;
#endif

    mca_btl_vader.super.btl_rdma_pipeline_send_length = mca_btl_vader.super.btl_eager_limit;
    mca_btl_vader.super.btl_rdma_pipeline_frag_size   = mca_btl_vader.super.btl_eager_limit;

#if OPAL_BTL_VADER_HAVE_XPMEM || OPAL_BTL_VADER_HAVE_CMA
    mca_btl_vader.super.btl_flags     = MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND_INPLACE;
#else
    mca_btl_vader.super.btl_flags     = MCA_BTL_FLAGS_SEND_INPLACE;
#endif

    mca_btl_vader.super.btl_seg_size  = sizeof (mca_btl_base_segment_t);
#if OPAL_BTL_VADER_HAVE_XPMEM || OPAL_BTL_VADER_HAVE_CMA
    mca_btl_vader.super.btl_bandwidth = 40000; /* Mbs */
#else
    mca_btl_vader.super.btl_bandwidth = 10000; /* Mbs */
#endif
    mca_btl_vader.super.btl_latency   = 1;     /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_vader_component.super.btl_version,
                                &mca_btl_vader.super);

    return OPAL_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_vader_component_open(void)
{
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_vader_component.vader_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_vader_component.vader_frags_user, ompi_free_list_t);
#if !OPAL_BTL_VADER_HAVE_XPMEM
    OBJ_CONSTRUCT(&mca_btl_vader_component.vader_frags_max_send, ompi_free_list_t);
#endif
    OBJ_CONSTRUCT(&mca_btl_vader_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_vader_component.pending_endpoints, opal_list_t);

    return OPAL_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_vader_component_close(void)
{
    OBJ_DESTRUCT(&mca_btl_vader_component.vader_frags_eager);
    OBJ_DESTRUCT(&mca_btl_vader_component.vader_frags_user);
#if !OPAL_BTL_VADER_HAVE_XPMEM
    OBJ_DESTRUCT(&mca_btl_vader_component.vader_frags_max_send);
#endif
    OBJ_DESTRUCT(&mca_btl_vader_component.lock);
    OBJ_DESTRUCT(&mca_btl_vader_component.pending_endpoints);

    if (NULL != mca_btl_vader_component.my_segment) {
        munmap (mca_btl_vader_component.my_segment, mca_btl_vader_component.segment_size);
    }

    return OPAL_SUCCESS;
}

static int mca_btl_base_vader_modex_send (void)
{
    struct vader_modex_t modex;
    int modex_size, rc;

#if OPAL_BTL_VADER_HAVE_XPMEM
    modex.seg_id = mca_btl_vader_component.my_seg_id;
    modex.segment_base = mca_btl_vader_component.my_segment;

    modex_size = sizeof (modex);
#else
    modex_size = opal_shmem_sizeof_shmem_ds (&mca_btl_vader_component.seg_ds);
    memmove (&modex.seg_ds, &mca_btl_vader_component.seg_ds, modex_size);
#endif

    OPAL_MODEX_SEND(rc, PMIX_SYNC_REQD, PMIX_LOCAL,
                    &mca_btl_vader_component.super.btl_version, &modex, modex_size);

    return rc;
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
    int rc;

    *num_btls = 0;

    /* disable if there are no local peers */
    if (0 == MCA_BTL_VADER_NUM_LOCAL_PEERS) {
        BTL_VERBOSE(("No peers to communicate with. Disabling vader."));
        return NULL;
    }

#if OPAL_BTL_VADER_HAVE_XPMEM
    /* limit segment alignment to be between 4k and 16M */
    if (component->log_attach_align < 12) {
        component->log_attach_align = 12;
    } else if (component->log_attach_align > 25) {
        component->log_attach_align = 25;
    }
#endif

    btls = (mca_btl_base_module_t **) calloc (1, sizeof (mca_btl_base_module_t *));
    if (NULL == btls) {
        return NULL;
    }

    /* ensure a sane segment size */
    if (component->segment_size < (2 << 20)) {
        component->segment_size = (2 << 20);
    }

    component->fbox_size = (component->fbox_size + MCA_BTL_VADER_FBOX_ALIGNMENT_MASK) & ~MCA_BTL_VADER_FBOX_ALIGNMENT_MASK;

    if (component->segment_size > (1ul << MCA_BTL_VADER_OFFSET_BITS)) {
        component->segment_size = 2ul << MCA_BTL_VADER_OFFSET_BITS;
    }

    /* no fast boxes allocated initially */
    component->num_fbox_in_endpoints = 0;
    component->fbox_count = 0;

#if OPAL_BTL_VADER_HAVE_XPMEM
    component->my_segment = mmap (NULL, component->segment_size, PROT_READ |
                                  PROT_WRITE, MAP_ANONYMOUS | MAP_SHARED, -1, 0);
    if ((void *)-1 == component->my_segment) {
        BTL_VERBOSE(("Could not create anonymous memory segment"));
        free (btls);
        return NULL;
    }

    /* create an xpmem segment for the entire memory space */
    component->my_seg_id = xpmem_make (0, VADER_MAX_ADDRESS, XPMEM_PERMIT_MODE, (void *)0666);
    if (-1 == component->my_seg_id) {
        BTL_VERBOSE(("Could not create xpmem segment"));
        free (btls);
        munmap (component->my_segment, component->segment_size);
        component->my_segment = NULL;
        return NULL;
    }
#else
    {
        char *sm_file;

        rc = asprintf(&sm_file, "%s" OPAL_PATH_SEP "vader_segment.%s.%d", opal_process_info.proc_session_dir,
                      opal_process_info.nodename, MCA_BTL_VADER_LOCAL_RANK);
        if (0 > rc) {
            free (btls);
            return NULL;
        }

        rc = opal_shmem_segment_create (&component->seg_ds, sm_file, component->segment_size);
        free (sm_file);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("Could not create shared memory segment"));
            free (btls);
            return NULL;
        }

        component->my_segment = opal_shmem_segment_attach (&component->seg_ds);
        if (NULL == component->my_segment) {
            BTL_VERBOSE(("Could not attach to just created shared memory segment"));
            goto failed;
        }
    }
#endif

    component->segment_offset = 0;

    /* initialize my fifo */
    vader_fifo_init ((struct vader_fifo_t *) component->my_segment);

    rc = mca_btl_base_vader_modex_send ();
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("Error sending modex"));
        goto failed;
    }

    *num_btls = 1;

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t *) &mca_btl_vader;

    /* set flag indicating btl not inited */
    mca_btl_vader.btl_inited = false;

    return btls;
failed:
#if OPAL_BTL_VADER_HAVE_XPMEM
    munmap (component->my_segment, component->segment_size);
#else
    opal_shmem_unlink (&component->seg_ds);
#endif

    if (btls) {
        free (btls);
    }

    return NULL;
}

void mca_btl_vader_poll_handle_frag (mca_btl_vader_hdr_t *hdr, struct mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_vader_frag_t frag = {.base = {.des_local = frag.segments, .des_local_count = 1}};
    const mca_btl_active_message_callback_t *reg;

    if (hdr->flags & MCA_BTL_VADER_FLAG_COMPLETE) {
        mca_btl_vader_frag_complete (hdr->frag);
        return;
    }

    reg = mca_btl_base_active_message_trigger + hdr->tag;
    frag.segments[0].seg_addr.pval = (void *) (hdr + 1);
    frag.segments[0].seg_len       = hdr->len;

    if (hdr->flags & MCA_BTL_VADER_FLAG_SINGLE_COPY) {
        mca_mpool_base_registration_t *xpmem_reg;

        xpmem_reg = vader_get_registation (endpoint, hdr->sc_iov.iov_base,
                                           hdr->sc_iov.iov_len, 0,
                                           &frag.segments[1].seg_addr.pval);

        frag.segments[1].seg_len       = hdr->sc_iov.iov_len;
        frag.base.des_local_count = 2;

        /* recv upcall */
        reg->cbfunc(&mca_btl_vader.super, hdr->tag, &frag.base, reg->cbdata);
        vader_return_registration (xpmem_reg, endpoint);
    } else {
        reg->cbfunc(&mca_btl_vader.super, hdr->tag, &frag.base, reg->cbdata);
    }

    if (OPAL_UNLIKELY(MCA_BTL_VADER_FLAG_SETUP_FBOX & hdr->flags)) {
        mca_btl_vader_endpoint_setup_fbox_recv (endpoint, relative2virtual(hdr->fbox_base));
        mca_btl_vader_component.fbox_in_endpoints[mca_btl_vader_component.num_fbox_in_endpoints++] = endpoint;
    }

    hdr->flags = MCA_BTL_VADER_FLAG_COMPLETE;
    vader_fifo_write_back (hdr, endpoint);
}

static inline int mca_btl_vader_poll_fifo (void)
{
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_vader_hdr_t *hdr;

    /* poll the fifo until it is empty or a limit has been hit (8 is arbitrary) */
    for (int fifo_count = 0 ; fifo_count < 8 ; ++fifo_count) {
        hdr = vader_fifo_read (mca_btl_vader_component.my_fifo, &endpoint);
        if (NULL == hdr) {
            return fifo_count;
        }

        mca_btl_vader_poll_handle_frag (hdr, endpoint);
    }

    return 1;
}

/**
 * Progress pending messages on an endpoint
 *
 * @param ep (IN)       Vader BTL endpoint
 */
static void mca_btl_vader_progress_waiting (mca_btl_base_endpoint_t *ep)
{
    mca_btl_vader_frag_t *frag;

    OPAL_THREAD_LOCK(&ep->lock);
    ep->waiting = false;
    while (NULL != (frag = (mca_btl_vader_frag_t *) opal_list_remove_first (&ep->pending_frags))) {
        OPAL_THREAD_UNLOCK(&ep->lock);
        if (!vader_fifo_write_ep (frag->hdr, ep)) {
            opal_list_prepend (&ep->pending_frags, (opal_list_item_t *) frag);
            opal_list_append (&mca_btl_vader_component.pending_endpoints, &ep->super);
            ep->waiting = true;
            break;
        }
        OPAL_THREAD_LOCK(&ep->lock);
    }
    OPAL_THREAD_UNLOCK(&ep->lock);
}

/**
 * Progress pending messages on all waiting endpoints
 *
 * @param ep (IN)       Vader BTL endpoint
 */
static void mca_btl_vader_progress_endpoints (void)
{
    int count;

    count = opal_list_get_size (&mca_btl_vader_component.pending_endpoints);

    for (int i = 0 ; i < count ; ++i) {
        mca_btl_vader_progress_waiting ((mca_btl_base_endpoint_t *) opal_list_remove_first (&mca_btl_vader_component.pending_endpoints));
    }
}

static int mca_btl_vader_component_progress (void)
{
    static int32_t lock = 0;
    int count = 0;

    if (opal_using_threads()) {
        if (opal_atomic_swap_32 (&lock, 1)) {
            return 0;
        }
    }

    /* check for messages in fast boxes */
    if (mca_btl_vader_component.num_fbox_in_endpoints) {
        count = mca_btl_vader_check_fboxes ();
    }

    mca_btl_vader_progress_endpoints ();

    if (VADER_FIFO_FREE == mca_btl_vader_component.my_fifo->fifo_head) {
        lock = 0;
        return count;
    }

    count += mca_btl_vader_poll_fifo ();
    opal_atomic_mb ();
    lock = 0;

    return count;
}
