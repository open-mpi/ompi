/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2022 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/btl/sm/btl_sm.h"
#include "opal/mca/btl/sm/btl_sm_fbox.h"
#include "opal/mca/btl/sm/btl_sm_fifo.h"
#include "opal/mca/btl/sm/btl_sm_frag.h"
#include "opal/mca/smsc/smsc.h"

#include <string.h>

static int sm_del_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers);

static int sm_register_error_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_module_error_cb_fn_t cbfunc);

static int sm_finalize(struct mca_btl_base_module_t *btl);

static struct mca_btl_base_descriptor_t *sm_prepare_src(struct mca_btl_base_module_t *btl,
                                                        struct mca_btl_base_endpoint_t *endpoint,
                                                        struct opal_convertor_t *convertor,
                                                        uint8_t order, size_t reserve, size_t *size,
                                                        uint32_t flags);

static int sm_add_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                        struct opal_bitmap_t *reachability);

mca_btl_sm_t mca_btl_sm = {
    {&mca_btl_sm_component.super, .btl_add_procs = sm_add_procs, .btl_del_procs = sm_del_procs,
     .btl_finalize = sm_finalize, .btl_alloc = mca_btl_sm_alloc, .btl_free = mca_btl_sm_free,
     .btl_prepare_src = sm_prepare_src, .btl_send = mca_btl_sm_send, .btl_sendi = mca_btl_sm_sendi,
     .btl_dump = mca_btl_base_dump, .btl_register_error = sm_register_error_cb}};

static int sm_btl_first_time_init(mca_btl_sm_t *sm_btl, int n)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    int rc;

    /* generate the endpoints */
    component->endpoints = (struct mca_btl_base_endpoint_t *)
        calloc(n + 1, sizeof(struct mca_btl_base_endpoint_t));
    if (NULL == component->endpoints) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    component->endpoints[n].peer_smp_rank = -1;

    component->fbox_in_endpoints = calloc(n + 1, sizeof(void *));
    if (NULL == component->fbox_in_endpoints) {
        free(component->endpoints);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    component->mpool = mca_mpool_basic_create((void *) (component->my_segment
                                                        + MCA_BTL_SM_FIFO_SIZE),
                                              (unsigned long) (mca_btl_sm_component.segment_size
                                                               - MCA_BTL_SM_FIFO_SIZE),
                                              64);
    if (NULL == component->mpool) {
        free(component->endpoints);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    rc = opal_free_list_init(&component->sm_fboxes, sizeof(opal_free_list_item_t), 8,
                             OBJ_CLASS(opal_free_list_item_t), mca_btl_sm_component.fbox_size,
                             opal_cache_line_size, 0, mca_btl_sm_component.fbox_max, 4,
                             component->mpool, 0, NULL, NULL, NULL);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* initialize fragment descriptor free lists */
    /* initialize free list for small send and inline fragments */
    rc = opal_free_list_init(&component->sm_frags_user, sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                             mca_btl_sm_component.max_inline_send + sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, component->sm_free_list_num,
                             component->sm_free_list_max, component->sm_free_list_inc,
                             component->mpool, 0, NULL, mca_btl_sm_frag_init,
                             &component->sm_frags_user);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* initialize free list for buffered send fragments */
    rc = opal_free_list_init(&component->sm_frags_eager, sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                             mca_btl_sm.super.btl_eager_limit + sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, component->sm_free_list_num,
                             component->sm_free_list_max, component->sm_free_list_inc,
                             component->mpool, 0, NULL, mca_btl_sm_frag_init,
                             &component->sm_frags_eager);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        /* initialize free list for buffered send fragments */
        rc = opal_free_list_init(&component->sm_frags_max_send, sizeof(mca_btl_sm_frag_t),
                                 opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                                 mca_btl_sm.super.btl_max_send_size + sizeof(mca_btl_sm_frag_t),
                                 opal_cache_line_size, component->sm_free_list_num,
                                 component->sm_free_list_max, component->sm_free_list_inc,
                                 component->mpool, 0, NULL, mca_btl_sm_frag_init,
                                 &component->sm_frags_max_send);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    /* set flag indicating btl has been inited */
    sm_btl->btl_inited = true;

    return OPAL_SUCCESS;
}

static int init_sm_endpoint(struct mca_btl_base_endpoint_t **ep_out, struct opal_proc_t *proc)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_sm_modex_t *modex;
    size_t msg_size;
    int rc;

    uint16_t peer_local_rank;
    uint16_t *ptr = &peer_local_rank;
    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_RANK, &proc->proc_name, &ptr, PMIX_UINT16);
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("could not read the local rank for peer. rc=%d", rc));
        return rc;
    }

    mca_btl_base_endpoint_t *ep = component->endpoints + peer_local_rank;
    *ep_out = ep;

    OBJ_CONSTRUCT(ep, mca_btl_sm_endpoint_t);

    ep->peer_smp_rank = peer_local_rank;

    if (!mca_btl_is_self_endpoint(ep)) {
        OPAL_MODEX_RECV_IMMEDIATE(rc, &component->super.btl_version, &proc->proc_name,
                                  (void **) &modex, &msg_size);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }

        /* attach to the remote segment */
        ep->smsc_endpoint = NULL;  /* assume no one sided support */
        if( NULL != mca_smsc ) {
            ep->smsc_endpoint = MCA_SMSC_CALL(get_endpoint, proc);
        }
        if (NULL == ep->smsc_endpoint) {
            /* disable RDMA */
            mca_btl_sm.super.btl_get = NULL;
            mca_btl_sm.super.btl_put = NULL;
            mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_RDMA;
        }
            /* store a copy of the segment information for detach */
            ep->seg_ds = malloc(modex->seg_ds_size);
            if (NULL == ep->seg_ds) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            memcpy(ep->seg_ds, &modex->seg_ds, modex->seg_ds_size);

            ep->segment_base = opal_shmem_segment_attach(ep->seg_ds);
            if (NULL == ep->segment_base) {
                return OPAL_ERROR;
            }

        OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);

        free(modex);
    } else {
        /* set up the segment base so we can calculate a virtual to real for local pointers */
        ep->segment_base = component->my_segment;
    }

    ep->fifo = (struct sm_fifo_t *) ep->segment_base;

    return OPAL_SUCCESS;
}

static int fini_sm_endpoint(struct mca_btl_base_endpoint_t *ep)
{
    /* check if the endpoint is initialized. avoids a double-destruct */
    if (ep->fifo) {
        OBJ_DESTRUCT(ep);
    }

    return OPAL_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 *
 * @param btl (IN)
 * @param proc (IN)
 * @param peer (OUT)
 * @return     OPAL_SUCCESS or error status on failure.
 *
 */

static int sm_add_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                        opal_bitmap_t *reachability)
{
    mca_btl_sm_t *sm_btl = (mca_btl_sm_t *) btl;
    const opal_proc_t *my_proc;
    int rc = OPAL_SUCCESS;

    /* initializion */

    /* get pointer to my proc structure */
    if (NULL == (my_proc = opal_proc_local_get())) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* jump out if there's not someone we can talk to */
    if (1 > MCA_BTL_SM_NUM_LOCAL_PEERS) {
        return OPAL_SUCCESS;
    }

    if (!sm_btl->btl_inited) {
        rc = sm_btl_first_time_init(sm_btl, 1 + MCA_BTL_SM_NUM_LOCAL_PEERS);
        if (rc != OPAL_SUCCESS) {
            return rc;
        }
    }

    for (int32_t proc = 0; proc < (int32_t) nprocs; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid
            || !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        if (my_proc != procs[proc] && NULL != reachability) {
            /* add this proc to shared memory accessibility list */
            rc = opal_bitmap_set_bit(reachability, proc);
            if (OPAL_SUCCESS != rc) {
                return rc;
            }
        }

        /* setup endpoint */
        rc = init_sm_endpoint(peers + proc, procs[proc]);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    return rc;
}

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */

static int sm_del_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers)
{
    for (size_t i = 0; i < nprocs; ++i) {
        if (peers[i]) {
            fini_sm_endpoint(peers[i]);
            peers[i] = NULL;
        }
    }

    return OPAL_SUCCESS;
}

/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

static int sm_finalize(struct mca_btl_base_module_t *btl)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_sm_t *sm_btl = (mca_btl_sm_t *) btl;

    if (!sm_btl->btl_inited) {
        return OPAL_SUCCESS;
    }

    for (int i = 0; i < (int) (1 + MCA_BTL_SM_NUM_LOCAL_PEERS); ++i) {
        fini_sm_endpoint(component->endpoints + i);
    }

    free(component->endpoints);
    component->endpoints = NULL;

    sm_btl->btl_inited = false;

    free(component->fbox_in_endpoints);
    component->fbox_in_endpoints = NULL;

    opal_shmem_unlink(&mca_btl_sm_component.seg_ds);
    opal_shmem_segment_detach(&mca_btl_sm_component.seg_ds);

    return OPAL_SUCCESS;
}

/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @param cbfunc (IN)  function to call on error
 * @return             Status indicating if cleanup was successful
 */
static int sm_register_error_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_module_error_cb_fn_t cbfunc)
{
    ((mca_btl_sm_t *) btl)->error_cb = cbfunc;
    return OPAL_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t *mca_btl_sm_alloc(struct mca_btl_base_module_t *btl,
                                            struct mca_btl_base_endpoint_t *endpoint, uint8_t order,
                                            size_t size, uint32_t flags)
{
    mca_btl_sm_frag_t *frag = NULL;

    if (size <= (size_t) mca_btl_sm_component.max_inline_send) {
        MCA_BTL_SM_FRAG_ALLOC_USER(frag, endpoint);
    } else if (size <= mca_btl_sm.super.btl_eager_limit) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
    } else if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
               && size <= mca_btl_sm.super.btl_max_send_size) {
        MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].seg_len = size;

        frag->base.des_flags = flags;
        frag->base.order = order;
    }

    return (mca_btl_base_descriptor_t *) frag;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_sm_free(struct mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des)
{
    MCA_BTL_SM_FRAG_RETURN((mca_btl_sm_frag_t *) des);

    return OPAL_SUCCESS;
}

/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
static struct mca_btl_base_descriptor_t *sm_prepare_src(struct mca_btl_base_module_t *btl,
                                                        struct mca_btl_base_endpoint_t *endpoint,
                                                        struct opal_convertor_t *convertor,
                                                        uint8_t order, size_t reserve, size_t *size,
                                                        uint32_t flags)
{
    const size_t total_size = reserve + *size;
    mca_btl_sm_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer(convertor, &data_ptr);
    assert(NULL != data_ptr);

    /* in place send fragment */
    if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor))) {
        uint32_t iov_count = 1;
        struct iovec iov;

        /* non-contiguous data requires the convertor */
        if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
            && total_size > mca_btl_sm.super.btl_eager_limit) {
            MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
        } else {
            MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
        }

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        iov.iov_len = *size;
        iov.iov_base = (IOVBASE_TYPE *) (((uintptr_t)(frag->segments[0].seg_addr.pval)) + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, size);
        if (OPAL_UNLIKELY(rc < 0)) {
            MCA_BTL_SM_FRAG_RETURN(frag);
            return NULL;
        }

        frag->segments[0].seg_len = *size + reserve;
    } else {
        if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
            if (OPAL_LIKELY(total_size <= mca_btl_sm.super.btl_eager_limit)) {
                MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
            } else {
                MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
            }
        } else {
            MCA_BTL_SM_FRAG_ALLOC_USER(frag, endpoint);
        }

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        /* use single-copy to send this segment if it is above the max inline send size */
        if (mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
            && total_size > (size_t) mca_btl_sm_component.max_inline_send) {
            /* single copy send */
            frag->hdr->flags = MCA_BTL_SM_FLAG_SINGLE_COPY;

            /* set up single copy io vector */
            frag->hdr->sc_iov.iov_base = data_ptr;
            frag->hdr->sc_iov.iov_len = *size;

            frag->segments[0].seg_len = reserve;
            frag->segments[1].seg_len = *size;
            frag->segments[1].seg_addr.pval = data_ptr;
            frag->base.des_segment_count = 2;
        } else {
            /* NTH: the covertor adds some latency so we bypass it here */
            memcpy((void *) ((uintptr_t) frag->segments[0].seg_addr.pval + reserve), data_ptr,
                   *size);
            frag->segments[0].seg_len = total_size;
        }
    }

    frag->base.order = order;
    frag->base.des_flags = flags;

    return &frag->base;
}

static void mca_btl_sm_endpoint_constructor(mca_btl_sm_endpoint_t *ep)
{
    OBJ_CONSTRUCT(&ep->pending_frags, opal_list_t);
    OBJ_CONSTRUCT(&ep->pending_frags_lock, opal_mutex_t);
    ep->fifo = NULL;
    ep->fbox_out.fbox = NULL;
}

static void mca_btl_sm_endpoint_destructor(mca_btl_sm_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->pending_frags);
    OBJ_DESTRUCT(&ep->pending_frags_lock);

    if (ep->seg_ds) {
        opal_shmem_ds_t seg_ds;

        /* opal_shmem_segment_detach expects a opal_shmem_ds_t and will
         * stomp past the end of the seg_ds if it is too small (which
         * ep->seg_ds probably is) */
        memcpy(&seg_ds, ep->seg_ds, opal_shmem_sizeof_shmem_ds(ep->seg_ds));
        free(ep->seg_ds);
        ep->seg_ds = NULL;

        /* disconnect from the peer's segment */
        opal_shmem_segment_detach(&seg_ds);
    }

    if (ep->fbox_out.fbox) {
        opal_free_list_return(&mca_btl_sm_component.sm_fboxes, ep->fbox_out.fbox);
    }

    if (ep->smsc_endpoint) {
        MCA_SMSC_CALL(return_endpoint, ep->smsc_endpoint);
        ep->smsc_endpoint = NULL;
    }

    ep->fbox_in.buffer = ep->fbox_out.buffer = NULL;
    ep->fbox_out.fbox = NULL;
    ep->segment_base = NULL;
    ep->fifo = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_sm_endpoint_t, opal_list_item_t, mca_btl_sm_endpoint_constructor,
                   mca_btl_sm_endpoint_destructor);
