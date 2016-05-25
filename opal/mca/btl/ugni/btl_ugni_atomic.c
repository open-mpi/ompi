/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_rdma.h"

static gni_fma_cmd_type_t amo_cmds[][MCA_BTL_ATOMIC_LAST] = {
    [OPAL_INT32] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_IADD_S,
        [MCA_BTL_ATOMIC_LAND] = GNI_FMA_ATOMIC2_AND_S,
        [MCA_BTL_ATOMIC_LOR] = GNI_FMA_ATOMIC2_OR_S,
        [MCA_BTL_ATOMIC_LXOR] = GNI_FMA_ATOMIC2_XOR_S,
        [MCA_BTL_ATOMIC_SWAP] = GNI_FMA_ATOMIC2_SWAP_S,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_IMIN_S,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_IMAX_S,
    },
    [OPAL_INT64] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC_ADD,
        [MCA_BTL_ATOMIC_AND] = GNI_FMA_ATOMIC_AND,
        [MCA_BTL_ATOMIC_OR] = GNI_FMA_ATOMIC_OR,
        [MCA_BTL_ATOMIC_XOR] = GNI_FMA_ATOMIC_XOR,
        [MCA_BTL_ATOMIC_SWAP] = GNI_FMA_ATOMIC2_SWAP,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_IMIN,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_IMAX,
    },
    [OPAL_FLOAT] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_FPADD_S,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FPMIN_S,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FPMAX_S,
    },
    [OPAL_DOUBLE] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_FPADD,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FPMIN,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FPMAX,
    },
};

static gni_fma_cmd_type_t famo_cmds[][MCA_BTL_ATOMIC_LAST] = {
    [OPAL_INT32] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_FIADD_S,
        [MCA_BTL_ATOMIC_LAND] = GNI_FMA_ATOMIC2_FAND_S,
        [MCA_BTL_ATOMIC_LOR] = GNI_FMA_ATOMIC2_FOR_S,
        [MCA_BTL_ATOMIC_LXOR] = GNI_FMA_ATOMIC2_FXOR_S,
        [MCA_BTL_ATOMIC_SWAP] = GNI_FMA_ATOMIC2_FSWAP_S,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FIMIN_S,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FIMAX_S,
    },
    [OPAL_INT64] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC_FADD,
        [MCA_BTL_ATOMIC_AND] = GNI_FMA_ATOMIC_FAND,
        [MCA_BTL_ATOMIC_OR] = GNI_FMA_ATOMIC_FOR,
        [MCA_BTL_ATOMIC_XOR] = GNI_FMA_ATOMIC_FXOR,
        [MCA_BTL_ATOMIC_SWAP] = GNI_FMA_ATOMIC2_FSWAP,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FIMIN,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FIMAX,
    },
    [OPAL_FLOAT] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_FFPADD_S,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FFPMIN_S,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FFPMAX_S,
    },
    [OPAL_DOUBLE] = {
        [MCA_BTL_ATOMIC_ADD] = GNI_FMA_ATOMIC2_FFPADD,
        [MCA_BTL_ATOMIC_MIN] = GNI_FMA_ATOMIC2_FFPMIN,
        [MCA_BTL_ATOMIC_MAX] = GNI_FMA_ATOMIC2_FFPMAX,
    },
};

int mca_btl_ugni_aop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                      mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                      mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    gni_mem_handle_t dummy = {0, 0};
    mca_btl_ugni_post_descriptor_t *post_desc;
    int gni_op, rc, type;
    size_t size;

    size = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? 4 : 8;
    if (MCA_BTL_ATOMIC_FLAG_FLOAT & flags) {
        type = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? OPAL_FLOAT : OPAL_DOUBLE;
    } else {
        type = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? OPAL_INT32 : OPAL_INT64;
    }

    gni_op = amo_cmds[type][op];
    if (0 == gni_op) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, NULL, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, 0, dummy, remote_address,
                        remote_handle->gni_handle, size, 0);
    post_desc->desc.base.amo_cmd = gni_op;

    post_desc->desc.base.first_operand = operand;

    OPAL_THREAD_LOCK(&endpoint->btl->device->dev_lock);
    rc = GNI_PostFma (endpoint->rdma_ep_handle, &post_desc->desc.base);
    OPAL_THREAD_UNLOCK(&endpoint->btl->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        mca_btl_ugni_return_post_descriptor (endpoint->btl, post_desc);
        if (GNI_RC_ILLEGAL_OP == rc) {
            return OPAL_ERR_NOT_SUPPORTED;
        }
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
    int gni_op, rc, type;
    size_t size;

    size = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? 4 : 8;
    if (MCA_BTL_ATOMIC_FLAG_FLOAT & flags) {
        type = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? OPAL_FLOAT : OPAL_DOUBLE;
    } else {
        type = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? OPAL_INT32 : OPAL_INT64;
    }

    gni_op = famo_cmds[type][op];
    if (0 == gni_op) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }


    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, (intptr_t) local_address, local_handle->gni_handle,
                        remote_address, remote_handle->gni_handle, size, 0);
    post_desc->desc.base.amo_cmd = gni_op;

    post_desc->desc.base.first_operand = operand;

    OPAL_THREAD_LOCK(&endpoint->btl->device->dev_lock);
    rc = GNI_PostFma (endpoint->rdma_ep_handle, &post_desc->desc.base);
    OPAL_THREAD_UNLOCK(&endpoint->btl->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        mca_btl_ugni_return_post_descriptor (endpoint->btl, post_desc);
        if (GNI_RC_ILLEGAL_OP == rc) {
            return OPAL_ERR_NOT_SUPPORTED;
        }
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
    int gni_op, rc;
    size_t size;

    gni_op = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? GNI_FMA_ATOMIC2_CSWAP_S : GNI_FMA_ATOMIC_CSWAP;
    size = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? 4 : 8;

    rc = mca_btl_ugni_check_endpoint_state_rdma (endpoint);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mca_btl_ugni_alloc_post_descriptor (endpoint, local_handle, cbfunc, cbcontext, cbdata, &post_desc);
    if (OPAL_UNLIKELY(NULL == post_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }


    init_gni_post_desc (&post_desc->desc, order, GNI_POST_AMO, (intptr_t) local_address, local_handle->gni_handle,
                        remote_address, remote_handle->gni_handle, size, 0);
    post_desc->desc.base.amo_cmd = gni_op;

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
