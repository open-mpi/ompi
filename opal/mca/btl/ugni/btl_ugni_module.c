/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
    }
};

int
mca_btl_ugni_module_init (mca_btl_ugni_module_t *ugni_module,
                          opal_common_ugni_device_t *dev)
{
    int rc;

    BTL_VERBOSE(("binding module %p to device %p", (void *) ugni_module,
                 (void *) dev));

    /* copy module defaults (and function pointers) */
    memmove (ugni_module, &mca_btl_ugni_module, sizeof (mca_btl_ugni_module));

    ugni_module->initialized = false;
    ugni_module->nlocal_procs = 0;
    ugni_module->active_send_count = 0;

    OBJ_CONSTRUCT(&ugni_module->failed_frags, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->failed_frags_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&ugni_module->eager_get_pending, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_get_pending_lock,opal_mutex_t);

    OBJ_CONSTRUCT(&ugni_module->eager_frags_send, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_frags_recv, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->smsg_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_int_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->pending_smsg_frags_bb, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->ep_wait_list_lock,opal_mutex_t);
    OBJ_CONSTRUCT(&ugni_module->ep_wait_list, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->endpoints, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->id_to_endpoint, opal_hash_table_t);
    OBJ_CONSTRUCT(&ugni_module->smsg_mboxes, opal_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->pending_descriptors, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_get_pending, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->post_descriptors, opal_free_list_t);

    ugni_module->device = dev;
    dev->btl_ctx = (void *) ugni_module;

    /* create wildcard endpoint to listen for connections.
     * there is no need to bind this endpoint. */
    OPAL_THREAD_LOCK(&dev->dev_lock);
    rc = GNI_EpCreate (ugni_module->device->dev_handle, NULL,
                       &ugni_module->wildcard_ep);
    OPAL_THREAD_UNLOCK(&dev->dev_lock);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating wildcard ugni endpoint"));
        return opal_common_rc_ugni_to_opal (rc);
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
    void *node;
    int rc;

    while (ugni_module->active_send_count) {
        /* ensure all sends are complete before closing the module */
        rc = mca_btl_ugni_progress_local_smsg (ugni_module);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    /* close all open connections and release endpoints */
    if (ugni_module->initialized) {
        rc = opal_hash_table_get_first_key_uint64 (&ugni_module->id_to_endpoint, &key, (void **) &ep, &node);
        while (OPAL_SUCCESS == rc) {
            if (NULL != ep) {
                mca_btl_ugni_release_ep (ep);
            }

            rc = opal_hash_table_get_next_key_uint64 (&ugni_module->id_to_endpoint, &key, (void **) &ep, node, &node);
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            mca_btl_ugni_kill_progress_thread();
        }

        /* destroy all cqs */
        OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
        rc = GNI_CqDestroy (ugni_module->rdma_local_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down local BTE/FMA CQ - %s",gni_err_str[rc]));
        }

        rc = GNI_CqDestroy (ugni_module->smsg_local_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down TX SMSG CQ - %s",gni_err_str[rc]));
        }

        rc = GNI_CqDestroy (ugni_module->smsg_remote_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down RX SMSG CQ - %s",gni_err_str[rc]));
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            rc = GNI_CqDestroy (ugni_module->rdma_local_irq_cq);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error tearing down local BTE/FMA CQ - %s",gni_err_str[rc]));
            }

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
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
    }

    OBJ_DESTRUCT(&ugni_module->eager_frags_send);
    OBJ_DESTRUCT(&ugni_module->eager_frags_recv);
    OBJ_DESTRUCT(&ugni_module->smsg_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_int_frags);
    OBJ_DESTRUCT(&ugni_module->ep_wait_list);
    OBJ_DESTRUCT(&ugni_module->smsg_mboxes);
    OBJ_DESTRUCT(&ugni_module->pending_smsg_frags_bb);
    OBJ_DESTRUCT(&ugni_module->id_to_endpoint);
    OBJ_DESTRUCT(&ugni_module->endpoints);

    OBJ_DESTRUCT(&ugni_module->eager_get_pending);
    OBJ_DESTRUCT(&ugni_module->eager_get_pending_lock);

    if (ugni_module->initialized) {
        /* need to tear down the mpools *after* the free lists */
        if (NULL != ugni_module->smsg_mpool) {
            (void) mca_mpool_base_module_destroy (ugni_module->smsg_mpool);
            ugni_module->smsg_mpool  = NULL;
        }

        if (NULL != ugni_module->super.btl_mpool) {
            (void) mca_mpool_base_module_destroy (ugni_module->super.btl_mpool);
            ugni_module->super.btl_mpool = NULL;
        }
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

    if (size <=  mca_btl_ugni_component.smsg_max_data) {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_SMSG(endpoint, frag);
    } else if (size <= btl->btl_eager_limit) {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(endpoint, frag);
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
    return mca_btl_ugni_prepare_src_send (btl, endpoint, convertor,
                                          order, reserve, size, flags);
}

static mca_btl_base_registration_handle_t *
mca_btl_ugni_register_mem (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *base,
                           size_t size, uint32_t flags)
{
    mca_btl_ugni_reg_t *reg;
    int rc;

    rc = btl->btl_mpool->mpool_register(btl->btl_mpool, base, size, 0,
                                        (mca_mpool_base_registration_t **) &reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return NULL;
    }

    return &reg->handle;
}

static int mca_btl_ugni_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_ugni_reg_t *reg =
        (mca_btl_ugni_reg_t *)((intptr_t) handle - offsetof (mca_btl_ugni_reg_t, handle));

    (void) btl->btl_mpool->mpool_deregister (btl->btl_mpool, &reg->base);

    return OPAL_SUCCESS;
}
