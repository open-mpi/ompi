/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_prepare.h"
#include "btl_ugni_smsg.h"

static int
mca_btl_ugni_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des);

static int
mca_btl_ugni_module_finalize (struct mca_btl_base_module_t* btl);

static struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src (struct mca_btl_base_module_t *btl,
                          struct mca_btl_base_endpoint_t *endpoint,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags);

static mca_btl_base_registration_handle_t *
mca_btl_ugni_register_mem (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *base,
                           size_t size, uint32_t flags);

static int mca_btl_ugni_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle);

mca_btl_ugni_module_t mca_btl_ugni_module = {
    .super = {
        .btl_component      = &mca_btl_ugni_component.super,
        .btl_add_procs      = mca_btl_ugni_add_procs,
        .btl_del_procs      = mca_btl_ugni_del_procs,
        .btl_finalize       = mca_btl_ugni_module_finalize,
        .btl_alloc          = mca_btl_ugni_alloc,
        .btl_free           = mca_btl_ugni_free,
        .btl_prepare_src    = mca_btl_ugni_prepare_src,
        .btl_send           = mca_btl_ugni_send,
        .btl_sendi          = mca_btl_ugni_sendi,
        .btl_put            = mca_btl_ugni_put,
        .btl_get            = mca_btl_ugni_get,
        .btl_register_mem   = mca_btl_ugni_register_mem,
        .btl_deregister_mem = mca_btl_ugni_deregister_mem,
        .btl_atomic_op      = mca_btl_ugni_aop,
        .btl_atomic_fop     = mca_btl_ugni_afop,
        .btl_atomic_cswap   = mca_btl_ugni_acswap,
        .btl_flush          = mca_btl_ugni_flush,
    }
};

static void mca_btl_ugni_datagram_event (int foo, short bar, void *arg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) arg;
    mca_btl_ugni_device_t *device = ugni_module->devices;
    struct timeval tv = {.tv_sec = 0, .tv_usec = MCA_BTL_UGNI_CONNECT_USEC};

    mca_btl_ugni_progress_datagram (device);

    opal_event_evtimer_add (&ugni_module->connection_event, &tv);
}

int
mca_btl_ugni_module_init (mca_btl_ugni_module_t *ugni_module)
{
    int rc;

    BTL_VERBOSE(("binding module %p to device 0", (void *) ugni_module));

    /* copy module defaults (and function pointers) */
    memmove (ugni_module, &mca_btl_ugni_module, sizeof (mca_btl_ugni_module));

    ugni_module->initialized = false;
    ugni_module->nlocal_procs = 0;
    ugni_module->active_datagrams = 0;
    ugni_module->active_rdma_count = 0;

    opal_event_evtimer_set (opal_sync_event_base, &ugni_module->connection_event,
                            mca_btl_ugni_datagram_event, ugni_module);

    OBJ_CONSTRUCT(&ugni_module->failed_frags, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->failed_frags_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&ugni_module->eager_get_pending, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_get_pending_lock,opal_mutex_t);

    for (int i = 0 ; i < MCA_BTL_UGNI_LIST_MAX ; ++i) {
        OBJ_CONSTRUCT(ugni_module->frags_lists + i, opal_free_list_t);
    }

    OBJ_CONSTRUCT(&ugni_module->pending_smsg_frags_bb, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->ep_wait_list_lock,opal_mutex_t);
    OBJ_CONSTRUCT(&ugni_module->ep_wait_list, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ugni_module->endpoints, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->id_to_endpoint, opal_hash_table_t);
    OBJ_CONSTRUCT(&ugni_module->smsg_mboxes, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_get_pending, opal_list_t);

    /* set up virtual device handles */
    for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
        rc = mca_btl_ugni_device_init (ugni_module->devices + i, i);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_VERBOSE(("error initializing uGNI device handle"));
            return rc;
        }
    }

    /* create wildcard endpoint on first device to listen for connections.
     * there is no need to bind this endpoint. We are single threaded
     * here so there is no need for a device lock. */
    rc = GNI_EpCreate (ugni_module->devices[0].dev_handle, NULL,
                       &ugni_module->wildcard_ep);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating wildcard ugni endpoint"));
        return mca_btl_rc_ugni_to_opal (rc);
    }

    /* post wildcard datagram */
    rc = mca_btl_ugni_wildcard_ep_post (ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error posting wildcard datagram"));
        return rc;
    }

    return OPAL_SUCCESS;
}

static int
mca_btl_ugni_module_finalize (struct mca_btl_base_module_t *btl)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *)btl;
    mca_btl_base_endpoint_t *ep;
    uint64_t key;
    int rc;

    if (ugni_module->initialized) {
        /* close all open connections and release endpoints */
        OPAL_HASH_TABLE_FOREACH(key, uint64, ep, &ugni_module->id_to_endpoint) {
            if (NULL != ep) {
                mca_btl_ugni_release_ep (ep);
            }
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            mca_btl_ugni_kill_progress_thread();
        }

        /* destroy all cqs */
        rc = GNI_CqDestroy (ugni_module->smsg_remote_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down RX SMSG CQ - %s",gni_err_str[rc]));
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            rc = GNI_CqDestroy (ugni_module->smsg_remote_irq_cq);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error tearing down remote SMSG CQ - %s",gni_err_str[rc]));
            }
        }

        /* cancel wildcard post */
        rc = GNI_EpPostDataCancelById (ugni_module->wildcard_ep,
                                       MCA_BTL_UGNI_CONNECT_WILDCARD_ID |
                                       OPAL_PROC_MY_NAME.vpid);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni error cancelling wildcard post"));
        }

        /* tear down wildcard endpoint */
        rc = GNI_EpDestroy (ugni_module->wildcard_ep);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni error destroying endpoint - %s",gni_err_str[rc]));
        }

        opal_event_del (&ugni_module->connection_event);
    }

    for (int i = 0 ; i < MCA_BTL_UGNI_LIST_MAX ; ++i) {
        OBJ_DESTRUCT(ugni_module->frags_lists + i);
    }

    OBJ_DESTRUCT(&ugni_module->ep_wait_list);
    OBJ_DESTRUCT(&ugni_module->smsg_mboxes);
    OBJ_DESTRUCT(&ugni_module->pending_smsg_frags_bb);
    OBJ_DESTRUCT(&ugni_module->id_to_endpoint);
    OBJ_DESTRUCT(&ugni_module->endpoint_lock);
    OBJ_DESTRUCT(&ugni_module->endpoints);

    OBJ_DESTRUCT(&ugni_module->eager_get_pending);
    OBJ_DESTRUCT(&ugni_module->eager_get_pending_lock);

    if (ugni_module->rcache) {
        mca_rcache_base_module_destroy (ugni_module->rcache);
    }

    for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
        mca_btl_ugni_device_fini (ugni_module->devices + i);
    }

    ugni_module->initialized = false;

    return OPAL_SUCCESS;
}


mca_btl_base_descriptor_t *
mca_btl_ugni_alloc(struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *endpoint,
                   uint8_t order, size_t size, uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag = NULL;

    /* do not allocate a fragment unless the wait list is relatively small. this
     * reduces the potential for resource exhaustion. note the wait list only exists
     * because we have no way to notify the sender that credits are available. */
    if (OPAL_UNLIKELY(opal_list_get_size (&endpoint->frag_wait_list) > 32)) {
        return NULL;
    }

    if (size <= mca_btl_ugni_component.smsg_max_data) {
        frag = mca_btl_ugni_frag_alloc_smsg (endpoint);
    } else if (size <= btl->btl_eager_limit) {
        frag = mca_btl_ugni_frag_alloc_eager_send (endpoint);
    }

    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    BTL_VERBOSE(("btl/ugni_module allocated frag of size: %u, flags: %x. frag = %p",
                 (unsigned int)size, flags, (void *) frag));

    frag->base.des_flags = flags;
    frag->base.order = order;
    frag->base.des_segments = &frag->segments[1];
    frag->base.des_segment_count = 1;

    frag->segments[0].seg_addr.pval = NULL;
    frag->segments[0].seg_len       = 0;
    frag->segments[1].seg_addr.pval = frag->base.super.ptr;
    frag->segments[1].seg_len       = size;

    frag->flags = MCA_BTL_UGNI_FRAG_BUFFERED;
    if (size > mca_btl_ugni_component.smsg_max_data) {
        mca_btl_ugni_reg_t *registration;

        frag->hdr_size = sizeof (frag->hdr.eager);
        frag->flags    |= MCA_BTL_UGNI_FRAG_EAGER | MCA_BTL_UGNI_FRAG_IGNORE;

        registration = (mca_btl_ugni_reg_t *) frag->base.super.registration;

        frag->hdr.eager.memory_handle = registration->handle;
    } else {
        frag->hdr_size = sizeof (frag->hdr.send);
    }

    return &frag->base;
}

static int
mca_btl_ugni_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des)
{
    return mca_btl_ugni_frag_return ((mca_btl_ugni_base_frag_t *) des);
}

static struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src (struct mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags)
{
    /* do not allocate a fragment unless the wait list is relatively small. this
     * reduces the potential for resource exhaustion. note the wait list only exists
     * because we have no way to notify the sender that credits are available. */
    if (OPAL_UNLIKELY(opal_list_get_size (&endpoint->frag_wait_list) > 32)) {
        return NULL;
    }

    return mca_btl_ugni_prepare_src_send (btl, endpoint, convertor,
                                          order, reserve, size, flags);
}

static mca_btl_base_registration_handle_t *
mca_btl_ugni_register_mem (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *base,
                           size_t size, uint32_t flags)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_reg_t *reg;
    int access_flags = flags & MCA_BTL_REG_FLAG_ACCESS_ANY;
    int rc;

    rc = ugni_module->rcache->rcache_register (ugni_module->rcache, base, size, 0, access_flags,
                                               (mca_rcache_base_registration_t **) &reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return NULL;
    }

    return &reg->handle;
}

static int mca_btl_ugni_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_reg_t *reg =
        (mca_btl_ugni_reg_t *)((intptr_t) handle - offsetof (mca_btl_ugni_reg_t, handle));

    (void) ugni_module->rcache->rcache_deregister (ugni_module->rcache, &reg->base);

    return OPAL_SUCCESS;
}

int mca_btl_ugni_event_fatal_error (gni_return_t grc, gni_cq_entry_t event_data)
{
    /* combined error check for get event and get completed. we might miss exactly
     * what happened but it is unrecoverable anyway. fwiw, this error path has
     * never been seen in production. */
    if (GNI_CQ_OVERRUN(event_data)) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("CQ overrun detected in RDMA event data. can not recover"));
    } else {
        BTL_ERROR(("Error in GNI_GetComplete %s", gni_err_str[grc]));
    }

    return mca_btl_rc_ugni_to_opal (grc);
}

int mca_btl_ugni_device_handle_event_error (mca_btl_ugni_rdma_desc_t *rdma_desc, gni_cq_entry_t event_data)
{
    mca_btl_ugni_device_t *device = rdma_desc->device;
    uint32_t recoverable = 1;

    (void) GNI_CqErrorRecoverable (event_data, &recoverable);

    if (OPAL_UNLIKELY(++rdma_desc->tries >= mca_btl_ugni_component.rdma_max_retries || !recoverable)) {
        char char_buffer[1024];
        GNI_CqErrorStr (event_data, char_buffer, sizeof (char_buffer));

        BTL_ERROR(("giving up on desciptor %p, recoverable %d: %s", (void *) rdma_desc, recoverable, char_buffer));

        return OPAL_ERROR;
    }

    return _mca_btl_ugni_repost_rdma_desc_device (device, rdma_desc);
}
