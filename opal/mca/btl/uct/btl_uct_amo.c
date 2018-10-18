/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_device_context.h"

#if OPAL_HAVE_UCT_EP_ATOMIC64_POST
/* we add 1 to the ops to differentiate between unsupported and supported ops since
 * UCT_ATOMIC_OP_ADD == 0. otherwise we would have to fill in this table completely. */
static int mca_btl_uct_btl_to_uct_atomic[MCA_BTL_ATOMIC_LAST] = {
    [MCA_BTL_ATOMIC_ADD] = UCT_ATOMIC_OP_ADD + 1,
    [MCA_BTL_ATOMIC_AND] = UCT_ATOMIC_OP_AND + 1,
    [MCA_BTL_ATOMIC_OR]  = UCT_ATOMIC_OP_OR + 1,
    [MCA_BTL_ATOMIC_XOR] = UCT_ATOMIC_OP_XOR + 1,
    [MCA_BTL_ATOMIC_SWAP] = UCT_ATOMIC_OP_SWAP + 1,
};
#endif

int mca_btl_uct_afop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                      void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                      uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                      void *cbcontext, void *cbdata)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_rdma_context (uct_btl);
    mca_btl_uct_uct_completion_t *comp = NULL;
    ucs_status_t ucs_status;
    uct_rkey_bundle_t rkey;
    uct_ep_h ep_handle;
    int rc;

#if OPAL_HAVE_UCT_EP_ATOMIC64_POST
    int uct_op = mca_btl_uct_btl_to_uct_atomic[op];

    if (OPAL_UNLIKELY(0 == uct_op--)) {
        return OPAL_ERR_BAD_PARAM;
    }
#else
    if (OPAL_UNLIKELY(MCA_BTL_ATOMIC_ADD != op && MCA_BTL_ATOMIC_SWAP != op)) {
        return OPAL_ERR_BAD_PARAM;
    }
#endif

    if (cbfunc) {
        comp = mca_btl_uct_uct_completion_alloc (uct_btl, endpoint, local_address, local_handle, context,
                                                 cbfunc, cbcontext, cbdata);
        if (OPAL_UNLIKELY(NULL == comp)) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    rc = mca_btl_uct_get_rkey (uct_btl, context, endpoint, remote_handle, &rkey, &ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        mca_btl_uct_uct_completion_release (comp);
        return rc;
    }

    mca_btl_uct_context_lock (context);

#if OPAL_HAVE_UCT_EP_ATOMIC64_POST
    if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
        ucs_status = uct_ep_atomic32_fetch (ep_handle, uct_op, operand, (uint32_t *) local_address, remote_address,
	                                    rkey.rkey, &comp->uct_comp);
    } else {
        ucs_status = uct_ep_atomic64_fetch (ep_handle, uct_op, operand, (uint64_t *) local_address, remote_address,
	                                    rkey.rkey, &comp->uct_comp);
    }
#else
    if (MCA_BTL_ATOMIC_ADD == op) {
        if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
            ucs_status = uct_ep_atomic_fadd32 (ep_handle, (uint32_t) operand, remote_address,
                                               rkey.rkey, (uint32_t *) local_address, &comp->uct_comp);
        } else {
            ucs_status = uct_ep_atomic_fadd64 (ep_handle, operand, remote_address, rkey.rkey,
                                               (uint64_t *) local_address, &comp->uct_comp);
        }
    } else {
        if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
            ucs_status = uct_ep_atomic_swap32 (ep_handle, (uint32_t) operand, remote_address,
                                               rkey.rkey, (uint32_t *) local_address, &comp->uct_comp);
        } else {
            ucs_status = uct_ep_atomic_swap64 (ep_handle, operand, remote_address, rkey.rkey,
                                               (uint64_t *) local_address, &comp->uct_comp);
        }
    }
#endif

    /* go ahead and progress the worker while we have the lock */
    (void) uct_worker_progress (context->uct_worker);

    mca_btl_uct_context_unlock (context);

    mca_btl_uct_device_handle_completions (context);

    if (UCS_INPROGRESS == ucs_status) {
        rc = OPAL_SUCCESS;
    } else if (UCS_OK == ucs_status) {
        rc = 1;
        mca_btl_uct_uct_completion_release (comp);
    } else {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        mca_btl_uct_uct_completion_release (comp);
    }

    uct_rkey_release (&rkey);

    return rc;
}

int mca_btl_uct_aop (struct mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                     uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                     mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                     mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    /* this is static so it survives after this function returns. we don't care about the result */
    static uint64_t result;

    /* just use the fetching ops for now. there probably is a performance benefit to using
     * the non-fetching on some platforms but this is easier to implement quickly and it
     * guarantees remote completion. */
    return mca_btl_uct_afop (btl, endpoint, &result, remote_address, NULL, remote_handle, op,
                             operand, flags, order, cbfunc, cbcontext, cbdata);
}

int mca_btl_uct_acswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                        void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                        mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value, int flags,
                        int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_rdma_context (uct_btl);
    mca_btl_uct_uct_completion_t *comp = NULL;
    ucs_status_t ucs_status;
    uct_rkey_bundle_t rkey;
    uct_ep_h ep_handle;
    int rc;

    if (cbfunc) {
        comp = mca_btl_uct_uct_completion_alloc (uct_btl, endpoint, local_address, local_handle, context,
                                                 cbfunc, cbcontext, cbdata);
        if (OPAL_UNLIKELY(NULL == comp)) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    rc = mca_btl_uct_get_rkey (uct_btl, context, endpoint, remote_handle, &rkey, &ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        mca_btl_uct_uct_completion_release (comp);
        return rc;
    }

    mca_btl_uct_context_lock (context);

    if (flags & MCA_BTL_ATOMIC_FLAG_32BIT) {
        ucs_status = uct_ep_atomic_cswap32 (ep_handle, (uint32_t) compare, (uint32_t) value, remote_address,
                                            rkey.rkey, (uint32_t *) local_address, &comp->uct_comp);
    } else {
        ucs_status = uct_ep_atomic_cswap64 (ep_handle, compare, value, remote_address, rkey.rkey,
                                            (uint64_t *) local_address, &comp->uct_comp);
    }

    /* go ahead and progress the worker while we have the lock */
    (void) uct_worker_progress (context->uct_worker);

    mca_btl_uct_context_unlock (context);

    mca_btl_uct_device_handle_completions (context);

    if (UCS_INPROGRESS == ucs_status) {
        rc = OPAL_SUCCESS;
    } else if (UCS_OK == ucs_status) {
        rc = 1;
        mca_btl_uct_uct_completion_release (comp);
    } else {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        mca_btl_uct_uct_completion_release (comp);
    }

    uct_rkey_release (&rkey);

    return rc;
}
