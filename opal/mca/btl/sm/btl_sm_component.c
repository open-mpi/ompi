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
 * Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2021 Google, Inc. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/threads/mutex.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"

#include "opal/mca/btl/sm/btl_sm.h"
#include "opal/mca/btl/sm/btl_sm_fbox.h"
#include "opal/mca/btl/sm/btl_sm_fifo.h"
#include "opal/mca/btl/sm/btl_sm_frag.h"
#include "opal/mca/smsc/smsc.h"

#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif

#include <fcntl.h>
#include <sys/mman.h>

#ifdef HAVE_SYS_PRCTL_H
#    include <sys/prctl.h>
#endif

/* NTH: OS X does not define MAP_ANONYMOUS */
#if !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#endif

static int mca_btl_sm_component_progress(void);
static int mca_btl_sm_component_open(void);
static int mca_btl_sm_component_close(void);
static int mca_btl_sm_component_register(void);
static mca_btl_base_module_t **
mca_btl_sm_component_init(int *num_btls, bool enable_progress_threads, bool enable_mpi_threads);

/*
 * Shared Memory (SM) component instance.
 */
mca_btl_sm_component_t mca_btl_sm_component = {
    .super =
        {
            /* First, the mca_base_component_t struct containing meta information
               about the component itself */
            .btl_version =
                {
                    MCA_BTL_DEFAULT_VERSION("sm"),
                    .mca_open_component = mca_btl_sm_component_open,
                    .mca_close_component = mca_btl_sm_component_close,
                    .mca_register_component_params = mca_btl_sm_component_register,
                },
            .btl_data =
                {/* The component is checkpoint ready */
                 .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT},

            .btl_init = mca_btl_sm_component_init,
            .btl_progress = mca_btl_sm_component_progress,
        } /* end super */
};

static int mca_btl_sm_component_register(void)
{
    (void) mca_base_var_group_component_register(&mca_btl_sm_component.super.btl_version,
                                                 "Enhanced shared memory byte transport later");

    /* register SM component variables */
    mca_btl_sm_component.sm_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "free_list_num",
                                           "Initial number of fragments "
                                           "to allocate for shared memory communication.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.sm_free_list_num);
    mca_btl_sm_component.sm_free_list_max = 512;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "free_list_max",
                                           "Maximum number of fragments "
                                           "to allocate for shared memory communication.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.sm_free_list_max);
    mca_btl_sm_component.sm_free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "free_list_inc",
                                           "Number of fragments to create "
                                           "on each allocation.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.sm_free_list_inc);

    mca_btl_sm_component.memcpy_limit = 524288;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "memcpy_limit",
                                           "Message size to switch from using "
                                           "memmove to memcpy. The relative speed of these two "
                                           "routines can vary by size.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.memcpy_limit);

#if 64 == MCA_BTL_SM_BITNESS
    mca_btl_sm_component.segment_size = 1 << 24;
#else
    mca_btl_sm_component.segment_size = 1 << 22;
#endif
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "segment_size",
                                           "Maximum size of all shared "
#if 64 == MCA_BTL_SM_BITNESS
                                           "memory buffers (default: 16M)",
#else
                                           "memory buffers (default: 4M)",
#endif
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.segment_size);

    mca_btl_sm_component.max_inline_send = 256;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "max_inline_send",
                                           "Maximum size to transfer "
                                           "using copy-in copy-out semantics",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.max_inline_send);

    mca_btl_sm_component.fbox_threshold = 16;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "fbox_threshold",
                                           "Number of sends required "
                                           "before an eager send buffer is setup for a peer "
                                           "(default: 16)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.fbox_threshold);

    mca_btl_sm_component.fbox_max = 32;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "fbox_max",
                                           "Maximum number of eager send buffers "
                                           "to allocate (default: 32)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.fbox_max);

    mca_btl_sm_component.fbox_size = 4096;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version, "fbox_size",
                                           "Size of per-peer fast transfer buffers (default: 4k)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_sm_component.fbox_size);

    if (0 == access("/dev/shm", W_OK)) {
        mca_btl_sm_component.backing_directory = "/dev/shm";
    } else {
        mca_btl_sm_component.backing_directory = opal_process_info.job_session_dir;
    }
    (void) mca_base_component_var_register(
        &mca_btl_sm_component.super.btl_version, "backing_directory",
        "Directory to place backing files for shared memory communication. "
        "This directory should be on a local filesystem such as /tmp or "
        "/dev/shm (default: (linux) /dev/shm, (others) session directory)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_btl_sm_component.backing_directory);

    mca_btl_sm.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;

    mca_btl_sm.super.btl_eager_limit = 4 * 1024;
    mca_btl_sm.super.btl_rndv_eager_limit = 32 * 1024;
    mca_btl_sm.super.btl_max_send_size = 32 * 1024;
    mca_btl_sm.super.btl_min_rdma_pipeline_size = INT_MAX;

    mca_btl_sm.super.btl_rdma_pipeline_send_length = mca_btl_sm.super.btl_eager_limit;
    mca_btl_sm.super.btl_rdma_pipeline_frag_size = mca_btl_sm.super.btl_eager_limit;

    mca_btl_sm.super.btl_flags = MCA_BTL_FLAGS_SEND_INPLACE | MCA_BTL_FLAGS_SEND;

    mca_btl_sm.super.btl_bandwidth = 20000; /* Mbs */
    mca_btl_sm.super.btl_latency = 1; /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_sm_component.super.btl_version, &mca_btl_sm.super);

    return OPAL_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_sm_component_open(void)
{
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_eager, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_user, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_max_send, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_fboxes, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.pending_endpoints, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.pending_fragments, opal_list_t);

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_sm_component_close(void)
{
    OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_eager);
    OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_user);
    OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_max_send);
    OBJ_DESTRUCT(&mca_btl_sm_component.sm_fboxes);
    OBJ_DESTRUCT(&mca_btl_sm_component.lock);
    OBJ_DESTRUCT(&mca_btl_sm_component.pending_endpoints);
    OBJ_DESTRUCT(&mca_btl_sm_component.pending_fragments);

    mca_btl_sm_component.my_segment = NULL;

    if (mca_btl_sm_component.mpool) {
        mca_btl_sm_component.mpool->mpool_finalize(mca_btl_sm_component.mpool);
        mca_btl_sm_component.mpool = NULL;
    }

    return OPAL_SUCCESS;
}

static int mca_btl_base_sm_modex_send(void)
{
    mca_btl_sm_modex_t modex;
    int modex_size;

    modex_size = sizeof(modex) - sizeof(modex.seg_ds);

        modex.seg_ds_size = opal_shmem_sizeof_shmem_ds(&mca_btl_sm_component.seg_ds);
        memmove(&modex.seg_ds, &mca_btl_sm_component.seg_ds, modex.seg_ds_size);
        modex_size += modex.seg_ds_size;

    int rc;
    OPAL_MODEX_SEND(rc, PMIX_LOCAL, &mca_btl_sm_component.super.btl_version, &modex, modex_size);

    return rc;
}

static mca_btl_base_registration_handle_t *
mca_btl_sm_register_mem(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                        void *base, size_t size, uint32_t flags)
{
    return (mca_btl_base_registration_handle_t *) MCA_SMSC_CALL(register_region, base, size);
}

static int mca_btl_sm_deregister_mem_knem(struct mca_btl_base_module_t *btl,
                                          struct mca_btl_base_registration_handle_t *handle)
{
    MCA_SMSC_CALL(deregister_region, (void *) handle);
    return OPAL_SUCCESS;
}

/*
 *  SM component initialization
 */
static mca_btl_base_module_t **
mca_btl_sm_component_init(int *num_btls, bool enable_progress_threads, bool enable_mpi_threads)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_base_module_t **btls = NULL;
    int rc;

    *num_btls = 0;

    /* disable if there are no local peers */
    if (0 == MCA_BTL_SM_NUM_LOCAL_PEERS) {
        BTL_VERBOSE(("No peers to communicate with. Disabling sm."));
        return NULL;
    }

    btls = (mca_btl_base_module_t **) calloc(1, sizeof(mca_btl_base_module_t *));
    if (NULL == btls) {
        return NULL;
    }

    /* ensure a sane segment size */
    if (component->segment_size < (2 << 20)) {
        component->segment_size = (2 << 20);
    }

    component->fbox_size = (component->fbox_size + MCA_BTL_SM_FBOX_ALIGNMENT_MASK)
                           & ~MCA_BTL_SM_FBOX_ALIGNMENT_MASK;

    if (component->segment_size > (1ul << MCA_BTL_SM_OFFSET_BITS)) {
        component->segment_size = 2ul << MCA_BTL_SM_OFFSET_BITS;
    }

    /* no fast boxes allocated initially */
    component->num_fbox_in_endpoints = 0;

    bool have_smsc = (NULL != mca_smsc);
    if (have_smsc) {
        mca_btl_sm.super.btl_flags |= MCA_BTL_FLAGS_RDMA;
        mca_btl_sm.super.btl_get = mca_btl_sm_get;
        mca_btl_sm.super.btl_put = mca_btl_sm_put;

        mca_btl_sm.super.btl_bandwidth = 40000; /* Mbs */

        if (mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
            mca_btl_sm.super.btl_eager_limit = 32 * 1024;
            mca_btl_sm.super.btl_rndv_eager_limit = mca_btl_sm.super.btl_eager_limit;
            mca_btl_sm.super.btl_max_send_size = mca_btl_sm.super.btl_eager_limit;
            mca_btl_sm.super.btl_min_rdma_pipeline_size = INT_MAX;
        }
        if (mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
            ssize_t handle_size = mca_smsc_base_registration_data_size();
            if (handle_size > 0) {
                mca_btl_sm.super.btl_registration_handle_size = (size_t) handle_size;
                mca_btl_sm.super.btl_register_mem = mca_btl_sm_register_mem;
                mca_btl_sm.super.btl_deregister_mem = mca_btl_sm_deregister_mem_knem;
            } else {
                BTL_ERROR(("single-copy component requires registration but could not provide the "
                           "registration handle size"));
                have_smsc = false;
            }
        }
    }
    if (!have_smsc) {
        mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_RDMA;
        mca_btl_sm.super.btl_get = NULL;
        mca_btl_sm.super.btl_put = NULL;
    }

    char *sm_file;

    // Note: Use the node_rank not the local_rank for the backing file.
    // This makes the file unique even when recovering from failures.
    rc = opal_asprintf(&sm_file, "%s" OPAL_PATH_SEP "sm_segment.%s.%u.%x.%d",
                       mca_btl_sm_component.backing_directory, opal_process_info.nodename,
                       geteuid(), OPAL_PROC_MY_NAME.jobid, opal_process_info.my_node_rank);
    if (0 > rc) {
        free(btls);
        return NULL;
    }
    opal_pmix_register_cleanup(sm_file, false, false, false);

    rc = opal_shmem_segment_create(&component->seg_ds, sm_file, component->segment_size);
    free(sm_file);
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("Could not create shared memory segment"));
        free(btls);
        return NULL;
    }

    component->my_segment = opal_shmem_segment_attach(&component->seg_ds);
    if (NULL == component->my_segment) {
        BTL_VERBOSE(("Could not attach to just created shared memory segment"));
        goto failed;
    }

    /* initialize my fifo */
    sm_fifo_init((struct sm_fifo_t *) component->my_segment);

    rc = mca_btl_base_sm_modex_send();
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("Error sending modex"));
        goto failed;
    }

    *num_btls = 1;

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t *) &mca_btl_sm;

    /* set flag indicating btl not inited */
    mca_btl_sm.btl_inited = false;

    return btls;
failed:
    opal_shmem_unlink(&component->seg_ds);

    if (btls) {
        free(btls);
    }

    return NULL;
}

void mca_btl_sm_poll_handle_frag(mca_btl_sm_hdr_t *hdr, struct mca_btl_base_endpoint_t *endpoint)
{
    if (hdr->flags & MCA_BTL_SM_FLAG_COMPLETE) {
        mca_btl_sm_frag_complete(hdr->frag);
        return;
    }

    const mca_btl_active_message_callback_t *reg = mca_btl_base_active_message_trigger + hdr->tag;
    mca_btl_base_segment_t segments[2] = {
        [0] = {.seg_addr.pval = (void *) (hdr + 1), .seg_len = hdr->len}};
    mca_btl_base_receive_descriptor_t frag = {.endpoint = endpoint,
                                              .des_segments = segments,
                                              .des_segment_count = 1,
                                              .tag = hdr->tag,
                                              .cbdata = reg->cbdata};

    if (hdr->flags & MCA_BTL_SM_FLAG_SINGLE_COPY) {
        void *ctx = MCA_SMSC_CALL(map_peer_region, endpoint->smsc_endpoint, /*flags=*/0,
                                  hdr->sc_iov.iov_base, hdr->sc_iov.iov_len,
                                  &segments[1].seg_addr.pval);
        assert(NULL != ctx);

        segments[1].seg_len = hdr->sc_iov.iov_len;
        frag.des_segment_count = 2;

        /* recv upcall */
        reg->cbfunc(&mca_btl_sm.super, &frag);
        MCA_SMSC_CALL(unmap_peer_region, ctx);
    } else {
        reg->cbfunc(&mca_btl_sm.super, &frag);
    }

    if (OPAL_UNLIKELY(MCA_BTL_SM_FLAG_SETUP_FBOX & hdr->flags)) {
        mca_btl_sm_endpoint_setup_fbox_recv(endpoint, relative2virtual(hdr->fbox_base));
        mca_btl_sm_component.fbox_in_endpoints[mca_btl_sm_component.num_fbox_in_endpoints++]
            = endpoint;
    }

    hdr->flags = MCA_BTL_SM_FLAG_COMPLETE;
    sm_fifo_write_back(hdr, endpoint);
}

static int mca_btl_sm_poll_fifo(void)
{
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_sm_hdr_t *hdr;

    /* poll the fifo until it is empty or a limit has been hit (8 is arbitrary) */
    for (int fifo_count = 0; fifo_count < 31; ++fifo_count) {
        hdr = sm_fifo_read(mca_btl_sm_component.my_fifo, &endpoint);
        if (NULL == hdr) {
            return fifo_count;
        }

        mca_btl_sm_poll_handle_frag(hdr, endpoint);
    }

    return 1;
}

/**
 * Progress pending messages on an endpoint
 *
 * @param ep (IN)       Sm BTL endpoint
 *
 * This is called with the component lock held so the component lock does
 * not need to be acquired before modifying the pending_endpoints list.
 */
static void mca_btl_sm_progress_waiting(mca_btl_base_endpoint_t *ep)
{
    mca_btl_sm_frag_t *frag, *next;
    int ret = 1;

    if (OPAL_UNLIKELY(NULL == ep)) {
        return;
    }

    OPAL_THREAD_LOCK(&ep->pending_frags_lock);
    OPAL_LIST_FOREACH_SAFE (frag, next, &ep->pending_frags, mca_btl_sm_frag_t) {
        ret = sm_fifo_write_ep(frag->hdr, ep);
        if (!ret) {
            OPAL_THREAD_UNLOCK(&ep->pending_frags_lock);
            return;
        }

        (void) opal_list_remove_first(&ep->pending_frags);
    }

    ep->waiting = false;
    opal_list_remove_item(&mca_btl_sm_component.pending_endpoints, &ep->super);

    OPAL_THREAD_UNLOCK(&ep->pending_frags_lock);
}

/**
 * Progress pending messages on all waiting endpoints
 *
 * @param ep (IN)       Sm BTL endpoint
 */
static void mca_btl_sm_progress_endpoints(void)
{
    mca_btl_base_endpoint_t *ep, *next;
    int count;

    count = opal_list_get_size(&mca_btl_sm_component.pending_endpoints);
    if (OPAL_LIKELY(0 == count)) {
        return;
    }

    OPAL_THREAD_LOCK(&mca_btl_sm_component.lock);
    OPAL_LIST_FOREACH_SAFE (ep, next, &mca_btl_sm_component.pending_endpoints,
                            mca_btl_base_endpoint_t) {
        mca_btl_sm_progress_waiting(ep);
    }
    OPAL_THREAD_UNLOCK(&mca_btl_sm_component.lock);
}

static int mca_btl_sm_component_progress(void)
{
    static opal_atomic_int32_t lock = 0;
    int count = 0;

    if (opal_using_threads()) {
        if (opal_atomic_swap_32(&lock, 1)) {
            return 0;
        }
    }

    /* check for messages in fast boxes */
    if (mca_btl_sm_component.num_fbox_in_endpoints) {
        count = mca_btl_sm_check_fboxes();
    }

    mca_btl_sm_progress_endpoints();

    if (SM_FIFO_FREE == mca_btl_sm_component.my_fifo->fifo_head) {
        lock = 0;
        return count;
    }

    count += mca_btl_sm_poll_fifo();
    opal_atomic_mb();
    lock = 0;

    return count;
}
