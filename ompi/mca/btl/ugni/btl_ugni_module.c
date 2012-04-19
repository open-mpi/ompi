/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/show_help.h"
#include "opal/align.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/dpm/dpm.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_smsg.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

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
    {
        /* .btl_component = */                 &mca_btl_ugni_component.super,

        /* these are set in component_register */
        /* .btl_eager_limit = */               0,
        /* .btl_rndv_eager_limit = */          0,
        /* .btl_max_send_size = */             0,
        /* .btl_rdma_pipeline_send_length = */ 0,
        /* .btl_rdma_pipeline_frag_size = */   0,
        /* .btl_min_rdma_pipeline_size = */    0,
        /* .btl_exclusivity = */               0,
        /* .btl_latency = */                   0,
        /* .btl_bandwidth = */                 0,
        /* .btl_flags = */                     0,

        /* member functions */
        mca_btl_ugni_add_procs,
        mca_btl_ugni_del_procs,
        NULL, /* register */
        mca_btl_ugni_module_finalize,
        mca_btl_ugni_alloc,
        mca_btl_ugni_free,
        mca_btl_ugni_prepare_src,
        mca_btl_ugni_prepare_dst,
        mca_btl_ugni_send,
        mca_btl_ugni_sendi,
        mca_btl_ugni_put,
        mca_btl_ugni_get,
        NULL, /* mca_btl_base_dump, */
        NULL, /* mpool */
        NULL, /* mca_btl_ugni_register_error_cb - error callback registration */
        NULL, /* mca_btl_ugni_ft_event */
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

    OBJ_CONSTRUCT(&ugni_module->failed_frags, opal_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_frags_send, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->eager_frags_recv, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->smsg_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->rdma_int_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ugni_module->pending_smsg_frags_bb, opal_pointer_array_t);

    ugni_module->device = dev;
    ugni_module->endpoints = NULL;
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

    rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.cq_size,
                       0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->rdma_local_cq);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("error creating local BTE/FMA CQ"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.cq_size,
                       0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->smsg_local_cq);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("error creating local SMSG CQ"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.cq_size,
                       0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->smsg_remote_cq);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("error creating remote SMSG CQ"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    ugni_module->next_frag_id = 0;

    return OMPI_SUCCESS;
}

static int
mca_btl_ugni_module_finalize (struct mca_btl_base_module_t *btl)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *)btl;
    int rc, i;

    OBJ_DESTRUCT(&ugni_module->eager_frags_send);
    OBJ_DESTRUCT(&ugni_module->eager_frags_recv);
    OBJ_DESTRUCT(&ugni_module->smsg_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_frags);
    OBJ_DESTRUCT(&ugni_module->rdma_int_frags);

    /* close all open connections and release endpoints */
    if (NULL != ugni_module->endpoints) {
        for (i = 0 ; i < ugni_module->endpoint_count ; ++i) {
            if (ugni_module->endpoints[i]) {
                mca_btl_ugni_release_ep (ugni_module->endpoints[i]);
            }

            ugni_module->endpoints[i] = NULL;
        }

        free (ugni_module->endpoints);

        ugni_module->endpoint_count = 0;
        ugni_module->endpoints = NULL;
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
                                   ORTE_PROC_MY_NAME->vpid);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("btl/ugni error cancelling wildcard post"));
    }

    /* tear down wildcard endpoint */
    rc = GNI_EpDestroy (ugni_module->wildcard_ep);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("btl/ugni error destroying endpoint"));
    }

    (void) mca_mpool_base_module_destroy (ugni_module->smsg_mpool);
    ugni_module->smsg_mpool  = NULL;

    (void) mca_mpool_base_module_destroy (ugni_module->super.btl_mpool);
    ugni_module->super.btl_mpool = NULL;

    OBJ_DESTRUCT(&ugni_module->pending_smsg_frags_bb);

    OBJ_DESTRUCT(&ugni_module->failed_frags);

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
    frag->base.des_src = frag->segments + 1;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = frag->segments + 1;
    frag->base.des_dst_cnt = 1;

    frag->hdr_size = (size <= mca_btl_ugni_component.smsg_max_data) ? sizeof (frag->hdr.send) :
        sizeof (frag->hdr.eager);

    frag->segments[0].seg_addr.pval = NULL;
    frag->segments[0].seg_len       = 0;
    frag->segments[1].seg_addr.pval = frag->base.super.ptr;
    frag->segments[1].seg_len       = size;

    return &frag->base;
}

static int
mca_btl_ugni_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des)
{
    return mca_btl_ugni_frag_return ((mca_btl_ugni_base_frag_t *) des);
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send (struct mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags)
{
    bool use_eager_get = (*size + reserve) > mca_btl_ugni_component.smsg_max_data;
    mca_mpool_base_registration_t *registration = NULL;
    mca_btl_ugni_base_frag_t *frag = NULL;
    bool send_in_place;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    send_in_place = !(opal_convertor_need_buffers(convertor) ||
                      (use_eager_get && ((uintptr_t)data_ptr & 3)));

    if (OPAL_UNLIKELY(*size > btl->btl_eager_limit)) {
        *size = btl->btl_eager_limit;
    }

    if (OPAL_LIKELY(send_in_place)) {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        BTL_VERBOSE(("preparing src for send fragment. size = %u",
                     (unsigned int)(*size + reserve)));

        if (OPAL_UNLIKELY(true == use_eager_get)) {
            rc = btl->btl_mpool->mpool_register(btl->btl_mpool, data_ptr,
                                                *size, 0, &registration);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                mca_btl_ugni_frag_return (frag);
                return NULL;
            }

            frag->registration = (mca_btl_ugni_reg_t *) registration;
            memcpy ((void *) frag->segments[1].seg_key.key64,
                    (void *)&((mca_btl_ugni_reg_t *)registration)->memory_hdl,
                    sizeof (((mca_btl_ugni_reg_t *)registration)->memory_hdl));        
        }
    } else {
        uint32_t iov_count = 1;
        struct iovec iov;

        /* buffer the user's data */
        if (OPAL_LIKELY(!use_eager_get)) {
            (void) MCA_BTL_UGNI_FRAG_ALLOC_SMSG(endpoint, frag);
        } else {
            (void) MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(endpoint, frag);
        }

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        data_ptr = frag->base.super.ptr;

        iov.iov_len  = *size;
        iov.iov_base = (IOVBASE_TYPE *) data_ptr;

        rc = opal_convertor_pack (convertor, &iov, &iov_count, size);
        if (OPAL_UNLIKELY(rc < 0)) {
            mca_btl_ugni_frag_return (frag);
            return NULL;
        }

        if (true == use_eager_get) {
            registration = frag->base.super.registration;
            memcpy ((void *) frag->segments[1].seg_key.key64,
                    (void *)&((mca_btl_ugni_reg_t *)registration)->memory_hdl,
                    sizeof (((mca_btl_ugni_reg_t *)registration)->memory_hdl));        
        }
    }

    frag->hdr_size = reserve + (use_eager_get ? sizeof (frag->hdr.eager) : sizeof (frag->hdr.send));
    frag->segments[0].seg_addr.pval = use_eager_get ? frag->hdr.eager_ex.pml_header : frag->hdr.send_ex.pml_header;
    frag->segments[0].seg_len       = reserve;

    frag->segments[1].seg_addr.pval = data_ptr;
    frag->segments[1].seg_len       = *size;

    frag->base.des_src     = frag->segments;
    frag->base.des_src_cnt = 2;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

static struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src (struct mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag = NULL;
    void *data_ptr;
    int rc;

    if (OPAL_LIKELY(reserve)) {
        return mca_btl_ugni_prepare_src_send (btl, endpoint, convertor,
                                              order, reserve, size, flags);
    }

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    /*
     * For medium message use FMA protocols and for large message
     * use BTE protocols
     */
    /* No need to register while using FMA Put (registration is
     * non-null in get-- is this always true?) */
    if (*size >= mca_btl_ugni_component.ugni_fma_limit || (flags & MCA_BTL_DES_FLAGS_GET)) {
        if (NULL == registration) {
            rc = btl->btl_mpool->mpool_register(btl->btl_mpool, data_ptr,
                                                *size, 0, &registration);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                mca_btl_ugni_frag_return (frag);
                return NULL;
            }

            frag->registration = (mca_btl_ugni_reg_t *) registration;
        }

        memcpy ((void *) frag->segments[0].seg_key.key64,
                (void *)&((mca_btl_ugni_reg_t *)registration)->memory_hdl,
                sizeof (((mca_btl_ugni_reg_t *)registration)->memory_hdl));
    } else {
        memset ((void *) frag->segments[0].seg_key.key64, 0,
                sizeof (frag->segments[0].seg_key.key64));
    }

    frag->segments[0].seg_addr.pval = data_ptr;
    frag->segments[0].seg_len = reserve + *size;

    frag->base.des_src     = frag->segments;
    frag->base.des_src_cnt = 1;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
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

    memcpy ((void *) frag->segments[0].seg_key.key64,
            (void *)&((mca_btl_ugni_reg_t *)registration)->memory_hdl,
            sizeof (((mca_btl_ugni_reg_t *)registration)->memory_hdl));

    frag->segments[0].seg_len = *size;
    frag->segments[0].seg_addr.pval = data_ptr;

    frag->base.des_dst     = frag->segments;
    frag->base.des_dst_cnt = 1;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return (struct mca_btl_base_descriptor_t *) frag;
}
