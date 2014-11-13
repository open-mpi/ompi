/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_rdma.h"

static gni_fma_cmd_type_t famo_cmds[] = {
    [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC_FADD,
    [MCA_BTL_ATOMIC_AND] = GNI_FMA_ATOMIC_FAND,
    [MCA_BTL_ATOMIC_OR] = GNI_FMA_ATOMIC_FOR,
    [MCA_BTL_ATOMIC_XOR] = GNI_FMA_ATOMIC_FXOR,
};

static gni_fma_cmd_type_t amo_cmds[] = {
    [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC_ADD,
    [MCA_BTL_ATOMIC_AND] = GNI_FMA_ATOMIC_AND,
    [MCA_BTL_ATOMIC_OR] = GNI_FMA_ATOMIC_OR,
    [MCA_BTL_ATOMIC_XOR] = GNI_FMA_ATOMIC_XOR,
};

int mca_btl_ugni_aop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                      mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                      mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    gni_mem_handle_t dummy = {0, 0};
    mca_btl_ugni_post_descriptor_t *post_desc;
    int rc;

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, NULL, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, 0, dummy, remote_address,
                        remote_handle->gni_handle, 8, 0);
    post_desc->desc.base.amo_cmd = amo_cmds[op];

    post_desc->desc.base.first_operand = operand;

    OPAL_THREAD_LOCK(&endpoint->btl->device->dev_lock);
    rc = GNI_PostFma (endpoint->rdma_ep_handle, &post_desc->desc.base);
    OPAL_THREAD_UNLOCK(&endpoint->btl->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

int mca_btl_ugni_afop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                       void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                       mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                       uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                       void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t *post_desc;
    int rc;

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }


    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, (intptr_t) local_address, local_handle->gni_handle,
                        remote_address, remote_handle->gni_handle, 8, 0);
    post_desc->desc.base.amo_cmd = famo_cmds[op];

    post_desc->desc.base.first_operand = operand;

    OPAL_THREAD_LOCK(&endpoint->btl->device->dev_lock);
    rc = GNI_PostFma (endpoint->rdma_ep_handle, &post_desc->desc.base);
    OPAL_THREAD_UNLOCK(&endpoint->btl->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        mca_btl_ugni_return_post_descriptor (endpoint->btl, post_desc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

int mca_btl_ugni_acswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                         void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                         mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value, int flags,
                         int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_ugni_post_descriptor_t *post_desc;
    int rc;

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }


    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, (intptr_t) local_address, local_handle->gni_handle,
                        remote_address, remote_handle->gni_handle, 8, 0);
    post_desc->desc.base.amo_cmd = GNI_FMA_ATOMIC_CSWAP;

    post_desc->desc.base.first_operand = compare;
    post_desc->desc.base.second_operand = value;

    OPAL_THREAD_LOCK(&endpoint->btl->device->dev_lock);
    rc = GNI_PostFma (endpoint->rdma_ep_handle, &post_desc->desc.base);
    OPAL_THREAD_UNLOCK(&endpoint->btl->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        mca_btl_ugni_return_post_descriptor (endpoint->btl, post_desc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}
