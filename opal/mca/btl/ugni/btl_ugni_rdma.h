/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UGNI_RDMA_H)
#define MCA_BTL_UGNI_RDMA_H

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_device.h"

int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                  mca_btl_ugni_eager_ex_frag_hdr_t hdr,
                                  mca_btl_ugni_base_frag_t *frag);

static inline void init_gni_post_desc (mca_btl_ugni_post_descriptor_t *post_desc,
                                       int order, gni_post_type_t op_type,
                                       uint64_t lcl_addr,
                                       gni_mem_handle_t lcl_mdh,
                                       uint64_t rem_addr,
                                       gni_mem_handle_t rem_mdh,
                                       uint64_t bufsize,
                                       gni_cq_handle_t cq_hndl) {
    post_desc->desc.type            = op_type;
    post_desc->desc.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
    if (MCA_BTL_NO_ORDER == order) {
        post_desc->desc.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
    } else {
        post_desc->desc.dlvr_mode       = GNI_DLVMODE_NO_ADAPT;
    }
    post_desc->desc.local_addr      = (uint64_t) lcl_addr;
    post_desc->desc.local_mem_hndl  = lcl_mdh;
    post_desc->desc.remote_addr     = (uint64_t) rem_addr;
    post_desc->desc.remote_mem_hndl = rem_mdh;
    post_desc->desc.length          = bufsize;
    post_desc->desc.rdma_mode       = 0;
    post_desc->desc.src_cq_hndl     = cq_hndl;
    post_desc->tries                = 0;
}

static inline int mca_btl_ugni_post_fma (struct mca_btl_base_endpoint_t *endpoint, gni_post_type_t op_type,
                                         size_t size, void *local_address, uint64_t remote_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_registration_handle_t *remote_handle,
                                         int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t *post_desc;
    gni_mem_handle_t local_gni_handle = {0, 0};
    int rc;

    if (local_handle) {
        local_gni_handle = local_handle->gni_handle;
    }

    post_desc = mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Post descriptor (CQ is ignored for FMA transactions) -- The CQ associated with the endpoint
     * is used. */
    init_gni_post_desc (post_desc, order, op_type, (intptr_t) local_address, local_gni_handle,
                        remote_address, remote_handle->gni_handle, size, 0);

    rc = mca_btl_ugni_endpoint_post_fma (endpoint, post_desc);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        mca_btl_ugni_return_post_descriptor (post_desc);
    }

    return rc;
}

static inline int mca_btl_ugni_post_bte (mca_btl_base_endpoint_t *endpoint, gni_post_type_t op_type,
                                         size_t size, void *local_address, uint64_t remote_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_registration_handle_t *remote_handle,
                                         int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t *post_desc;
    int rc;

    post_desc = mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Post descriptor */
    init_gni_post_desc (post_desc, order, op_type, (intptr_t) local_address, local_handle->gni_handle,
                        remote_address, remote_handle->gni_handle, size, 0);

    rc = mca_btl_ugni_endpoint_post_rdma (endpoint, post_desc);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        mca_btl_ugni_return_post_descriptor (post_desc);
    }

    return rc;
}

static inline int mca_btl_ugni_post_cqwrite (mca_btl_base_endpoint_t *endpoint, mca_btl_ugni_cq_t *cq,
                                             gni_mem_handle_t irq_mhndl, uint64_t value,
                                             mca_btl_base_rdma_completion_fn_t cbfunc,
                                             void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t *post_desc;
    int rc;

    post_desc = mca_btl_ugni_alloc_post_descriptor (endpoint, NULL, cbfunc, cbcontext, cbdata);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    post_desc->desc.type = GNI_POST_CQWRITE;
    post_desc->desc.cqwrite_value = value;   /* up to 48 bytes here, not used for now */
    post_desc->desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
    post_desc->desc.dlvr_mode = GNI_DLVMODE_IN_ORDER;
    post_desc->desc.src_cq_hndl = cq->gni_handle;
    post_desc->desc.remote_mem_hndl = irq_mhndl;
    post_desc->tries = 0;
    post_desc->cq = cq;

    rc = mca_btl_ugni_endpoint_post_cqwrite (endpoint, post_desc);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {   /* errors for PostCqWrite treated as non-fatal */
        mca_btl_ugni_return_post_descriptor (post_desc);
    }

    return rc;
}

static inline int mca_btl_ugni_post (mca_btl_base_endpoint_t *endpoint, int get, size_t size,
                                     void *local_address, uint64_t remote_address,
                                     mca_btl_base_registration_handle_t *local_handle,
                                     mca_btl_base_registration_handle_t *remote_handle,
                                     int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                     void *cbcontext, void *cbdata)
{
    const gni_post_type_t fma_ops[2] = {GNI_POST_FMA_PUT, GNI_POST_FMA_GET};
    const gni_post_type_t rdma_ops[2] = {GNI_POST_RDMA_PUT, GNI_POST_RDMA_GET};

    if (size <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (endpoint, fma_ops[get], size, local_address, remote_address,
                                      local_handle, remote_handle, order, cbfunc, cbcontext, cbdata);
    }

    return mca_btl_ugni_post_bte (endpoint, rdma_ops[get], size, local_address, remote_address,
                                  local_handle, remote_handle, order, cbfunc, cbcontext, cbdata);
}

static inline int mca_btl_ugni_repost (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_post_descriptor_t *post_desc)
{
    if (GNI_POST_RDMA_PUT == post_desc->desc.type || GNI_POST_RDMA_GET == post_desc->desc.type) {
        return mca_btl_ugni_endpoint_post_rdma (post_desc->endpoint, post_desc);
    }

    return mca_btl_ugni_endpoint_post_fma (post_desc->endpoint, post_desc);
}

#endif /* MCA_BTL_UGNI_RDMA_H */
