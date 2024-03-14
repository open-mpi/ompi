/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc, All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ofi_rdma.h"
#include <rdma/fi_atomic.h>

static inline int to_fi_op(mca_btl_base_atomic_op_t op)
{
    switch (op) {
    case MCA_BTL_ATOMIC_ADD:
        return FI_SUM;
    case MCA_BTL_ATOMIC_SWAP:
        return FI_ATOMIC_WRITE;
    case MCA_BTL_ATOMIC_MAX:
        return FI_MAX;
    case MCA_BTL_ATOMIC_MIN:
        return FI_MIN;
    case MCA_BTL_ATOMIC_LAND:
        return FI_LAND;
    case MCA_BTL_ATOMIC_AND:
        return FI_BAND;
    case MCA_BTL_ATOMIC_LOR:
        return FI_LOR;
    case MCA_BTL_ATOMIC_OR:
        return FI_BOR;
    case MCA_BTL_ATOMIC_LXOR:
        return FI_LXOR;
    case MCA_BTL_ATOMIC_XOR:
        return FI_BXOR;

    default:
        BTL_ERROR(("Unknown or unsupported atomic op."));
        MCA_BTL_OFI_ABORT();

        /* just to squash the warning */
        return OPAL_ERROR;
    }
}

int mca_btl_ofi_afop(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                     void *local_address, uint64_t remote_address,
                     mca_btl_base_registration_handle_t *local_handle,
                     mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                     uint64_t operand, int flags, int order,
                     mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    int rc;
    int fi_datatype = FI_UINT64;
    int fi_op;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *btl_endpoint = (mca_btl_ofi_endpoint_t *) endpoint;
    mca_btl_ofi_rdma_completion_t *comp = NULL;
    mca_btl_ofi_context_t *ofi_context;

    MCA_BTL_OFI_NUM_RDMA_INC(ofi_btl);
    ofi_context = get_ofi_context(ofi_btl);

    if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
        fi_datatype = FI_UINT32;
    }

    fi_op = to_fi_op(op);

    comp = mca_btl_ofi_rdma_completion_alloc(btl, endpoint, ofi_context, local_address,
                                             local_handle, cbfunc, cbcontext, cbdata,
                                             MCA_BTL_OFI_TYPE_AFOP);

    /* copy the operand because it might get freed from upper layer */
    comp->operand = (uint64_t) operand;

    remote_address = (remote_address - (uint64_t) remote_handle->base_addr);

    rc = fi_fetch_atomic(ofi_context->tx_ctx, (void *) &comp->operand, 1, NULL, /* operand */
                         local_address, local_handle->desc,                     /* results */
                         btl_endpoint->peer_addr,                               /* remote addr */
                         remote_address, remote_handle->rkey,                   /* remote buffer */
                         fi_datatype, fi_op, &comp->comp_ctx);

    if (rc == -FI_EAGAIN) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    } else if (rc < 0) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        BTL_ERROR(("fi_fetch_atomic failed with rc=%d (%s)", rc, fi_strerror(-rc)));
        MCA_BTL_OFI_ABORT();
    }

    return OPAL_SUCCESS;
}

int mca_btl_ofi_aop(struct mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                    uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                    mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    int rc;
    int fi_datatype = FI_UINT64;
    int fi_op;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *btl_endpoint = (mca_btl_ofi_endpoint_t *) endpoint;
    mca_btl_ofi_rdma_completion_t *comp = NULL;
    mca_btl_ofi_context_t *ofi_context;

    MCA_BTL_OFI_NUM_RDMA_INC(ofi_btl);
    ofi_context = get_ofi_context(ofi_btl);

    if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
        fi_datatype = FI_UINT32;
    }

    fi_op = to_fi_op(op);

    comp = mca_btl_ofi_rdma_completion_alloc(btl, endpoint, ofi_context, NULL, NULL, cbfunc,
                                             cbcontext, cbdata, MCA_BTL_OFI_TYPE_AOP);

    /* copy the operand because it might get freed from upper layer */
    comp->operand = (uint64_t) operand;

    remote_address = (remote_address - (uint64_t) remote_handle->base_addr);

    rc = fi_atomic(ofi_context->tx_ctx, (void *) &comp->operand, 1, NULL, /* operand */
                   btl_endpoint->peer_addr,                               /* remote addr */
                   remote_address, remote_handle->rkey,                   /* remote buffer */
                   fi_datatype, fi_op, &comp->comp_ctx);

    if (rc == -FI_EAGAIN) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    } else if (rc < 0) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        BTL_ERROR(("fi_atomic failed with rc=%d (%s)", rc, fi_strerror(-rc)));
        MCA_BTL_OFI_ABORT();
    }

    return OPAL_SUCCESS;
}

int mca_btl_ofi_acswap(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                       void *local_address, uint64_t remote_address,
                       mca_btl_base_registration_handle_t *local_handle,
                       mca_btl_base_registration_handle_t *remote_handle, uint64_t compare,
                       uint64_t value, int flags, int order,
                       mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    int rc;
    int fi_datatype = FI_UINT64;

    mca_btl_ofi_rdma_completion_t *comp = NULL;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *btl_endpoint = (mca_btl_ofi_endpoint_t *) endpoint;
    mca_btl_ofi_context_t *ofi_context;

    MCA_BTL_OFI_NUM_RDMA_INC(ofi_btl);
    ofi_context = get_ofi_context(ofi_btl);

    if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
        fi_datatype = FI_UINT32;
    }

    comp = mca_btl_ofi_rdma_completion_alloc(btl, endpoint, ofi_context, local_address,
                                             local_handle, cbfunc, cbcontext, cbdata,
                                             MCA_BTL_OFI_TYPE_CSWAP);

    /* copy the operand because it might get freed from upper layer */
    comp->operand = (uint64_t) value;
    comp->compare = (uint64_t) compare;

    remote_address = (remote_address - (uint64_t) remote_handle->base_addr);

    /* perform atomic */
    rc = fi_compare_atomic(ofi_context->tx_ctx, (void *) &comp->operand, 1, NULL,
                           (void *) &comp->compare, NULL, local_address, local_handle->desc,
                           btl_endpoint->peer_addr, remote_address, remote_handle->rkey,
                           fi_datatype, FI_CSWAP, &comp->comp_ctx);

    if (rc == -FI_EAGAIN) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    } else if (rc < 0) {
        MCA_BTL_OFI_NUM_RDMA_DEC(ofi_btl);
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t *) comp);
        BTL_ERROR(("fi_compare_atomic failed with rc=%d (%s)", rc, fi_strerror(-rc)));
        MCA_BTL_OFI_ABORT();
    }

    return OPAL_SUCCESS;
}
