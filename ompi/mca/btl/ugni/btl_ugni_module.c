/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

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

static mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_dst (mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          opal_convertor_t *convertor, uint8_t order,
                          size_t reserve, size_t *size, uint32_t flags);

static struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src (struct mca_btl_base_module_t *btl,
                          struct mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags);

mca_btl_ugni_module_t mca_btl_ugni_module = {
    .super = {
        .btl_component   = &mca_btl_ugni_component.super,
        .btl_add_procs   = mca_btl_ugni_add_procs,
        .btl_del_procs   = mca_btl_ugni_del_procs,
        .btl_finalize    = mca_btl_ugni_module_finalize,
        .btl_alloc       = mca_btl_ugni_alloc,
        .btl_free        = mca_btl_ugni_free,
        .btl_prepare_src = mca_btl_ugni_prepare_src,
        .btl_prepare_dst = mca_btl_ugni_prepare_dst,
        .btl_send        = mca_btl_ugni_send,
        .btl_sendi       = mca_btl_ugni_sendi,
        .btl_put         = mca_btl_ugni_put,
        .btl_get         = mca_btl_ugni_get,
    }
};

int
mca_btl_ugni_module_init (mca_btl_ugni_module_t *ugni_module,
                          ompi_common_ugni_device_t *dev)
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
    OBJ_CONSTRUCT(&ugni_module->eager_frags_send, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_frags_recv, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->smsg_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_int_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->pending_smsg_frags_bb, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->ep_wait_list, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->endpoints, opal_pointer_array_t);
    OBJ_CONSTRUCT(&ugni_module->id_to_endpoint, opal_hash_table_t);

    ugni_module->device = dev;
    dev->btl_ctx = (void *) ugni_module;

    /* create wildcard endpoint to listen for connections.
     * there is no need to bind this endpoint. */
    rc = GNI_EpCreate (ugni_module->device->dev_handle, NULL,
                       &ugni_module->wildcard_ep);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        BTL_ERROR(("error creating wildcard ugni endpoint"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    /* post wildcard datagram */
    rc = mca_btl_ugni_wildcard_ep_post (ugni_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        BTL_ERROR(("error posting wildcard datagram"));
        return rc;
    }

    return OMPI_SUCCESS;
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
        if (OMPI_SUCCESS != rc) {
            break;
        }
    }

    OBJ_DESTRUCT(&ugni_module->eager_frags_send);
    OBJ_DESTRUCT(&ugni_module->eager_frags_recv);
    OBJ_DESTRUCT(&ugni_module->smsg_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_int_frags);
    OBJ_DESTRUCT(&ugni_module->ep_wait_list);

    /* close all open connections and release endpoints */
    if (ugni_module->initialized) {
        rc = opal_hash_table_get_first_key_uint64 (&ugni_module->id_to_endpoint, &key, (void **) &ep, &node);
        while (OPAL_SUCCESS == rc) {
            if (NULL != ep) {
                mca_btl_ugni_release_ep (ep);
            }

            rc = opal_hash_table_get_next_key_uint64 (&ugni_module->id_to_endpoint, &key, (void **) &ep, node, &node);
        }

        /* destroy all cqs */
        rc = GNI_CqDestroy (ugni_module->rdma_local_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down local BTE/FMA CQ"));
        }

        rc = GNI_CqDestroy (ugni_module->smsg_local_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down local SMSG CQ"));
        }

        rc = GNI_CqDestroy (ugni_module->smsg_remote_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error tearing down remote SMSG CQ"));
        }

        /* cancel wildcard post */
        rc = GNI_EpPostDataCancelById (ugni_module->wildcard_ep,
                                       MCA_BTL_UGNI_CONNECT_WILDCARD_ID |
                                       OMPI_PROC_MY_NAME->vpid);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni error cancelling wildcard post"));
        }

        /* tear down wildcard endpoint */
        rc = GNI_EpDestroy (ugni_module->wildcard_ep);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni error destroying endpoint"));
        }
    }

    OBJ_DESTRUCT(&ugni_module->pending_smsg_frags_bb);
    OBJ_DESTRUCT(&ugni_module->id_to_endpoint);
    OBJ_DESTRUCT(&ugni_module->endpoints);
    OBJ_DESTRUCT(&ugni_module->failed_frags);

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

    return OMPI_SUCCESS;
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
    frag->base.des_src = &frag->segments[1].base;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = &frag->segments[1].base;
    frag->base.des_dst_cnt = 1;

    frag->segments[0].base.seg_addr.pval = NULL;
    frag->segments[0].base.seg_len       = 0;
    frag->segments[1].base.seg_addr.pval = frag->base.super.ptr;
    frag->segments[1].base.seg_len       = size;

    frag->flags = MCA_BTL_UGNI_FRAG_BUFFERED;
    if (size > mca_btl_ugni_component.smsg_max_data) {
        mca_btl_ugni_reg_t *registration;

        frag->hdr_size = sizeof (frag->hdr.eager);
        frag->flags    |= MCA_BTL_UGNI_FRAG_EAGER | MCA_BTL_UGNI_FRAG_IGNORE;

        registration = (mca_btl_ugni_reg_t *) frag->base.super.registration;

        frag->segments[1].memory_handle = registration->memory_hdl;
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
                          mca_mpool_base_registration_t *registration,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags)
{
    if (OPAL_LIKELY(reserve)) {
        return mca_btl_ugni_prepare_src_send (btl, endpoint, convertor,
                                              order, reserve, size, flags);
    } else {
        return mca_btl_ugni_prepare_src_rdma (btl, endpoint, registration,
                                              convertor, order, size, flags);
    }
}

static mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_dst (mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          opal_convertor_t *convertor, uint8_t order,
                          size_t reserve, size_t *size, uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    /* always need to register the buffer for put/get (even for fma) */
    if (NULL == registration) {
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                                            data_ptr, *size, 0,
                                            &registration);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            mca_btl_ugni_frag_return (frag);
            return NULL;
        }

        frag->registration = (mca_btl_ugni_reg_t*) registration;
    }

    frag->segments[0].memory_handle      = ((mca_btl_ugni_reg_t *)registration)->memory_hdl;
    frag->segments[0].base.seg_len       = *size;
    frag->segments[0].base.seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;

    frag->base.des_dst     = &frag->segments->base;
    frag->base.des_dst_cnt = 1;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return (struct mca_btl_base_descriptor_t *) frag;
}
