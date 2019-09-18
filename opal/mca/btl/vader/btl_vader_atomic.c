/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_xpmem.h"

int mca_btl_vader_emu_aop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                           uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                           mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                           mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;
    return mca_btl_vader_rdma_frag_start (btl, endpoint, MCA_BTL_VADER_OP_ATOMIC, operand, 0, op, order, flags,
                                          size, NULL, remote_address, cbfunc, cbcontext, cbdata);
}

int mca_btl_vader_emu_afop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                            void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                            mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                            uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                            void *cbcontext, void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;
    return mca_btl_vader_rdma_frag_start (btl, endpoint, MCA_BTL_VADER_OP_ATOMIC, operand, 0, op, order, flags,
                                          size, local_address, remote_address, cbfunc, cbcontext, cbdata);
}

int mca_btl_vader_emu_acswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                              void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                              mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value, int flags,
                              int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;
    return mca_btl_vader_rdma_frag_start (btl, endpoint, MCA_BTL_VADER_OP_CSWAP, compare, value, 0, order,
                                          flags, size, local_address, remote_address, cbfunc, cbcontext, cbdata);
}
