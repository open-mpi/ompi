/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSC_RDMA_BTL_COMM_H
#define OSC_RDMA_BTL_COMM_H

#include "osc_rdma_frag.h"

#include "opal/mca/btl/btl.h"


void ompi_osc_rdma_atomic_complete(mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                    void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                    void *context, void *data, int status);


static inline int
ompi_osc_rdma_btl_put(ompi_osc_rdma_module_t *module, uint8_t btl_index,
		      struct mca_btl_base_endpoint_t *endpoint,
		      void *local_address, uint64_t remote_address,
		      struct mca_btl_base_registration_handle_t *local_handle,
		      struct mca_btl_base_registration_handle_t *remote_handle,
		      size_t size, int flags, int order,
		      mca_btl_base_rdma_completion_fn_t cbfunc,
		      void *cbcontext, void *cbdata)
{
    if (module->use_accelerated_btl) {
        mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl(module, btl_index);
        return btl->btl_put(btl, endpoint, local_address, remote_address,
                            local_handle, remote_handle, size, flags, order,
                            cbfunc, cbcontext, cbdata);
    } else {
        mca_btl_base_am_rdma_module_t *am_rdma = ompi_osc_rdma_selected_am_rdma(module, btl_index);
        return am_rdma->am_btl_put(am_rdma, endpoint, local_address, remote_address,
                                   local_handle, remote_handle, size, flags, order,
                                   cbfunc, cbcontext, cbdata);
    }
}


static inline int
ompi_osc_rdma_btl_get(ompi_osc_rdma_module_t *module, uint8_t btl_index,
		      struct mca_btl_base_endpoint_t *endpoint,
                      void *local_address, uint64_t remote_address,
		      struct mca_btl_base_registration_handle_t *local_handle,
		      struct mca_btl_base_registration_handle_t *remote_handle,
		      size_t size, int flags, int order,
		      mca_btl_base_rdma_completion_fn_t cbfunc,
		      void *cbcontext, void *cbdata)
{

    if (module->use_accelerated_btl) {
        mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl(module, btl_index);
        return btl->btl_get(btl, endpoint, local_address, remote_address,
                            local_handle, remote_handle, size, flags, order,
                            cbfunc, cbcontext, cbdata);
    } else {
        mca_btl_base_am_rdma_module_t *am_rdma = ompi_osc_rdma_selected_am_rdma(module, btl_index);
        return am_rdma->am_btl_get(am_rdma, endpoint, local_address, remote_address,
                                   local_handle, remote_handle, size, flags, order,
                                   cbfunc, cbcontext, cbdata);
    }
}


static inline int
ompi_osc_rdma_btl_atomic_op(ompi_osc_rdma_module_t *module, uint8_t btl_index,
                            struct mca_btl_base_endpoint_t *endpoint,
                            uint64_t remote_address, struct mca_btl_base_registration_handle_t *remote_handle,
                            mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                            mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl(module, btl_index);

    /* the AM BTL interface does not currently support op calls */
    assert(module->use_accelerated_btl);

    return btl->btl_atomic_op(btl, endpoint, remote_address, remote_handle,
                              op, operand, flags, order,
                              cbfunc, cbcontext, cbdata);
}


static inline int
ompi_osc_rdma_btl_atomic_fop(ompi_osc_rdma_module_t *module, uint8_t btl_index,
                             struct mca_btl_base_endpoint_t *endpoint,
                             void *local_address, uint64_t remote_address,
                             struct mca_btl_base_registration_handle_t *local_handle,
                             struct mca_btl_base_registration_handle_t *remote_handle,
                             mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                             mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)

{
    if (module->use_accelerated_btl) {
        mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl(module, btl_index);
        return btl->btl_atomic_fop(btl, endpoint, local_address, remote_address,
                                   local_handle, remote_handle,
                                   op, operand, flags, order,
                                   cbfunc, cbcontext, cbdata);
    } else {
        mca_btl_base_am_rdma_module_t *am_rdma = ompi_osc_rdma_selected_am_rdma(module, btl_index);
        return am_rdma->am_btl_atomic_fop(am_rdma, endpoint, local_address, remote_address,
                                          local_handle, remote_handle,
                                          op, operand, flags, order,
                                          cbfunc, cbcontext, cbdata);
    }
}


static inline int
ompi_osc_rdma_btl_atomic_cswap(ompi_osc_rdma_module_t *module, uint8_t btl_index,
                               struct mca_btl_base_endpoint_t *endpoint,
                               void *local_address, uint64_t remote_address,
                               struct mca_btl_base_registration_handle_t *local_handle,
                               struct mca_btl_base_registration_handle_t *remote_handle,
                               uint64_t compare, uint64_t value, int flags, int order,
                               mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    if (module->use_accelerated_btl) {
        mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl(module, btl_index);
        return btl->btl_atomic_cswap(btl, endpoint, local_address, remote_address,
                                     local_handle, remote_handle,
                                     compare, value, flags, order,
                                     cbfunc, cbcontext, cbdata);
    } else {
        mca_btl_base_am_rdma_module_t *am_rdma = ompi_osc_rdma_selected_am_rdma(module, btl_index);
        return am_rdma->am_btl_atomic_cswap(am_rdma, endpoint, local_address, remote_address,
                                            local_handle, remote_handle,
                                            compare, value, flags, order,
                                            cbfunc, cbcontext, cbdata);
    }
}


static inline int
ompi_osc_rdma_btl_fop(ompi_osc_rdma_module_t *module, uint8_t btl_index,
		      struct mca_btl_base_endpoint_t *endpoint, uint64_t address,
		      mca_btl_base_registration_handle_t *address_handle, int op,
		      int64_t operand, int flags, int64_t *result, const bool wait_for_completion,
		      ompi_osc_rdma_pending_op_cb_fn_t cbfunc, void *cbdata, void *cbcontext)
{
    ompi_osc_rdma_pending_op_t *pending_op;
    mca_btl_base_module_t *selected_btl = ompi_osc_rdma_selected_btl (module, btl_index);
    int ret = OPAL_ERROR;

    pending_op = OBJ_NEW(ompi_osc_rdma_pending_op_t);
    assert (NULL != pending_op);

    if (!wait_for_completion) {
        /* NTH: need to keep track of pending ops to avoid a potential teardown problem */
        pending_op->module = module;
        (void) opal_atomic_fetch_add_32 (&module->pending_ops, 1);
    }

    pending_op->op_result = (void *) result;
    pending_op->op_size = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? 4 : 8;
    OBJ_RETAIN(pending_op);
    if (cbfunc) {
        pending_op->cbfunc = cbfunc;
        pending_op->cbdata = cbdata;
        pending_op->cbcontext = cbcontext;
    }

    /* spin until the btl has accepted the operation */
    do {
        if (NULL == pending_op->op_frag) {
            ret = ompi_osc_rdma_frag_alloc (module, 8, &pending_op->op_frag, (char **) &pending_op->op_buffer);
        }

        if (NULL != pending_op->op_frag) {
            ret = ompi_osc_rdma_btl_atomic_fop(module, btl_index, endpoint, pending_op->op_buffer,
                                               (intptr_t) address, pending_op->op_frag->handle, address_handle,
                                               op, operand, flags, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete,
                                               (void *) pending_op, NULL);
        }

        if (OPAL_LIKELY(!ompi_osc_rdma_oor(ret))) {
            break;
        }
        ompi_osc_rdma_progress (module);
    } while (1);

    if (OPAL_SUCCESS != ret) {
        if (OPAL_LIKELY(1 == ret)) {
            *result = ((int64_t *) pending_op->op_buffer)[0];
            ret = OMPI_SUCCESS;
            ompi_osc_rdma_atomic_complete (selected_btl, endpoint, pending_op->op_buffer,
                                           pending_op->op_frag->handle, (void *) pending_op, NULL, OPAL_SUCCESS);
        } else {
            /* need to release here because ompi_osc_rdma_atomic_complete was not called */
            OBJ_RELEASE(pending_op);
        }
    } else if (wait_for_completion) {
        while (!pending_op->op_complete) {
            ompi_osc_rdma_progress (module);
        }
    }

    OBJ_RELEASE(pending_op);

    return ret;
}


static inline int
ompi_osc_rdma_btl_op(ompi_osc_rdma_module_t *module, uint8_t btl_index,
		     struct mca_btl_base_endpoint_t *endpoint, uint64_t address,
		     mca_btl_base_registration_handle_t *address_handle,
		     int op, int64_t operand, int flags, const bool wait_for_completion,
		     ompi_osc_rdma_pending_op_cb_fn_t cbfunc, void *cbdata, void *cbcontext)
{
    ompi_osc_rdma_pending_op_t *pending_op;
    mca_btl_base_module_t *selected_btl = ompi_osc_rdma_selected_btl (module, btl_index);
    int ret;

    /* if using the AM RDMA interface with alternate BTLs or if the
       accelerated BTL does not support atomic ops, emulate the atomic
       op over a fetch and atomic op */
    if (!module->use_accelerated_btl || !(selected_btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_OPS)) {
        return ompi_osc_rdma_btl_fop (module, btl_index, endpoint, address, address_handle, op, operand, flags,
                                      NULL, wait_for_completion, cbfunc, cbdata, cbcontext);
    }

    pending_op = OBJ_NEW(ompi_osc_rdma_pending_op_t);
    assert (NULL != pending_op);
    OBJ_RETAIN(pending_op);
    if (cbfunc) {
        pending_op->cbfunc = cbfunc;
        pending_op->cbdata = cbdata;
        pending_op->cbcontext = cbcontext;
    }

    if (!wait_for_completion) {
        /* NTH: need to keep track of pending ops to avoid a potential teardown problem */
        pending_op->module = module;
        (void) opal_atomic_fetch_add_32 (&module->pending_ops, 1);
    }

    /* spin until the btl has accepted the operation */
    do {
        ret = ompi_osc_rdma_btl_atomic_op(module, btl_index, endpoint, (intptr_t) address, address_handle,
                                          op, operand, flags, MCA_BTL_NO_ORDER, ompi_osc_rdma_atomic_complete,
                                          (void *) pending_op, NULL);

        if (OPAL_LIKELY(!ompi_osc_rdma_oor(ret))) {
            break;
        }
        ompi_osc_rdma_progress (module);
    } while (1);

    if (OPAL_SUCCESS != ret) {
        /* need to release here because ompi_osc_rdma_atomic_complete was not called */
        OBJ_RELEASE(pending_op);
        if (OPAL_LIKELY(1 == ret)) {
            if (cbfunc) {
                cbfunc (cbdata, cbcontext, OMPI_SUCCESS);
            }
            ret = OMPI_SUCCESS;
        }
    } else if (wait_for_completion) {
        while (!pending_op->op_complete) {
            ompi_osc_rdma_progress (module);
        }
    }

    OBJ_RELEASE(pending_op);

    return ret;
}


static inline int
ompi_osc_rdma_btl_cswap(ompi_osc_rdma_module_t *module, uint8_t btl_index,
			struct mca_btl_base_endpoint_t *endpoint, uint64_t address,
			mca_btl_base_registration_handle_t *address_handle,
			int64_t compare, int64_t value, int flags, int64_t *result)
{
    ompi_osc_rdma_pending_op_t *pending_op;
    int ret;

    pending_op = OBJ_NEW(ompi_osc_rdma_pending_op_t);
    assert (NULL != pending_op);

    OBJ_RETAIN(pending_op);

    pending_op->op_result = (void *) result;
    pending_op->op_size = (MCA_BTL_ATOMIC_FLAG_32BIT & flags) ? 4 : 8;

    /* spin until the btl has accepted the operation */
    do {
        if (NULL == pending_op->op_frag) {
            ret = ompi_osc_rdma_frag_alloc (module, 8, &pending_op->op_frag, (char **) &pending_op->op_buffer);
        }
        if (NULL != pending_op->op_frag) {
            ret = ompi_osc_rdma_btl_atomic_cswap(module, btl_index, endpoint, pending_op->op_buffer,
                                                  address, pending_op->op_frag->handle, address_handle, compare,
                                                  value, flags, 0, ompi_osc_rdma_atomic_complete, (void *) pending_op,
                                                  NULL);
        }

        if (OPAL_LIKELY(!ompi_osc_rdma_oor(ret))) {
            break;
        }
        ompi_osc_rdma_progress (module);
    } while (1);

    if (OPAL_SUCCESS != ret) {
        if (OPAL_LIKELY(1 == ret)) {
            *result = ((int64_t *) pending_op->op_buffer)[0];
            ret = OMPI_SUCCESS;
        }

        /* need to release here because ompi_osc_rdma_atomic_complete was not called */
        OBJ_RELEASE(pending_op);
    } else {
        while (!pending_op->op_complete) {
            ompi_osc_rdma_progress (module);
        }
    }

    OBJ_RELEASE(pending_op);

    return ret;
}

#endif /* OSC_RDMA_BTL_COMM_H */
