/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
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

static inline void init_post_desc (mca_btl_ugni_post_descriptor_t *post_desc,
                                   mca_btl_base_endpoint_t *endpoint, int order,
                                   gni_post_type_t op_type, uint64_t lcl_addr,
                                   gni_mem_handle_t lcl_mdh, uint64_t rem_addr,
                                   gni_mem_handle_t rem_mdh, uint64_t bufsize,
                                   gni_cq_handle_t cq_hndl, mca_btl_base_rdma_completion_fn_t cbfunc,
                                   void *cbcontext, void *cbdata,
                                   mca_btl_base_registration_handle_t *local_handle) {
    post_desc->endpoint = endpoint;
    post_desc->cbfunc = cbfunc;
    post_desc->ctx = cbcontext;
    post_desc->cbdata = cbdata;
    post_desc->local_handle = local_handle;
    post_desc->gni_desc.type            = op_type;
    post_desc->gni_desc.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
    if (MCA_BTL_NO_ORDER == order) {
        post_desc->gni_desc.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
    } else {
        post_desc->gni_desc.dlvr_mode       = GNI_DLVMODE_NO_ADAPT;
    }
    post_desc->gni_desc.local_addr      = (uint64_t) lcl_addr;
    post_desc->gni_desc.local_mem_hndl  = lcl_mdh;
    post_desc->gni_desc.remote_addr     = (uint64_t) rem_addr;
    post_desc->gni_desc.remote_mem_hndl = rem_mdh;
    post_desc->gni_desc.length          = bufsize;
    post_desc->gni_desc.rdma_mode       = 0;
    post_desc->gni_desc.src_cq_hndl     = cq_hndl;
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_post_fma (struct mca_btl_base_endpoint_t *endpoint, gni_post_type_t op_type,
                                         size_t size, void *local_address, uint64_t remote_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_registration_handle_t *remote_handle,
                                         int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t post_desc;
    gni_mem_handle_t local_gni_handle = {0, 0};

    if (local_handle) {
        local_gni_handle = local_handle->gni_handle;
    }

    /* Post descriptor (CQ is ignored for FMA transactions) -- The CQ associated with the endpoint
     * is used. */
    init_post_desc (&post_desc, endpoint, order, op_type, (intptr_t) local_address, local_gni_handle,
                    remote_address, remote_handle->gni_handle, size, 0, cbfunc, cbcontext, cbdata,
                    local_handle);

    return mca_btl_ugni_endpoint_post_fma (endpoint, &post_desc);
}

static inline int mca_btl_ugni_post_bte (mca_btl_base_endpoint_t *endpoint, gni_post_type_t op_type,
                                         size_t size, void *local_address, uint64_t remote_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_registration_handle_t *remote_handle,
                                         int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata)
{
    mca_btl_ugni_module_t *module = mca_btl_ugni_ep_btl (endpoint);
    mca_btl_ugni_post_descriptor_t post_desc;
    int rc;

    /* There is a performance benefit to throttling the total number of active BTE tranactions. Not sure
     * what the optimium is but the limit is inforced as a soft limit. */
    if (module->active_rdma_count >= mca_btl_ugni_component.active_rdma_threshold) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (void) OPAL_THREAD_FETCH_ADD32 (&module->active_rdma_count, 1);

    /* Post descriptor */
    init_post_desc (&post_desc, endpoint, order, op_type, (intptr_t) local_address, local_handle->gni_handle,
                    remote_address, remote_handle->gni_handle, size, 0, cbfunc, cbcontext, cbdata,
                    local_handle);

    rc = mca_btl_ugni_endpoint_post_rdma (endpoint, &post_desc);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        (void) OPAL_THREAD_FETCH_ADD32 (&module->active_rdma_count, -1);
    }

    return rc;
}

static inline int mca_btl_ugni_post_cqwrite (mca_btl_base_endpoint_t *endpoint, mca_btl_ugni_cq_t *cq,
                                             gni_mem_handle_t irq_mhndl, uint64_t value,
                                             mca_btl_base_rdma_completion_fn_t cbfunc,
                                             void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t post_desc;

    post_desc.gni_desc.type = GNI_POST_CQWRITE;
    post_desc.gni_desc.cqwrite_value = value;   /* up to 48 bytes here, not used for now */
    post_desc.gni_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
    post_desc.gni_desc.dlvr_mode = GNI_DLVMODE_IN_ORDER;
    post_desc.gni_desc.src_cq_hndl = cq->gni_handle;
    post_desc.gni_desc.remote_mem_hndl = irq_mhndl;
    post_desc.cq = cq;

    return mca_btl_ugni_endpoint_post_cqwrite (endpoint, &post_desc);
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_post (mca_btl_base_endpoint_t *endpoint, int get, size_t size,
                                     void *local_address, uint64_t remote_address,
                                     mca_btl_base_registration_handle_t *local_handle,
                                     mca_btl_base_registration_handle_t *remote_handle,
                                     int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                                     void *cbcontext, void *cbdata)
{
    const gni_post_type_t fma_ops[2] = {GNI_POST_FMA_PUT, GNI_POST_FMA_GET};
    const gni_post_type_t rdma_ops[2] = {GNI_POST_RDMA_PUT, GNI_POST_RDMA_GET};
    const size_t fma_limit = (size_t) (get ? mca_btl_ugni_component.ugni_fma_get_limit :
                                       mca_btl_ugni_component.ugni_fma_put_limit);

    if (size <= fma_limit) {
        return mca_btl_ugni_post_fma (endpoint, fma_ops[get], size, local_address, remote_address,
                                      local_handle, remote_handle, order, cbfunc, cbcontext, cbdata);
    }

    return mca_btl_ugni_post_bte (endpoint, rdma_ops[get], size, local_address, remote_address,
                                  local_handle, remote_handle, order, cbfunc, cbcontext, cbdata);
}

#endif /* MCA_BTL_UGNI_RDMA_H */
