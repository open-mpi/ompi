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

void mca_btl_uct_uct_completion (uct_completion_t *uct_comp, ucs_status_t status)
{
    mca_btl_uct_uct_completion_t *comp = (mca_btl_uct_uct_completion_t *) ((uintptr_t) uct_comp - offsetof (mca_btl_uct_uct_completion_t, uct_comp));

    BTL_VERBOSE(("network operation complete. status = %d", status));

    comp->status = status;
    opal_fifo_push (&comp->dev_context->completion_fifo, &comp->super.super);
}


static void mca_btl_uct_uct_completion_construct (mca_btl_uct_uct_completion_t *comp)
{
    comp->frag = NULL;
    comp->uct_comp.func = mca_btl_uct_uct_completion;
}

OBJ_CLASS_INSTANCE(mca_btl_uct_uct_completion_t, opal_free_list_item_t, mca_btl_uct_uct_completion_construct, NULL);


mca_btl_uct_uct_completion_t *
mca_btl_uct_uct_completion_alloc (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
                                  void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                  mca_btl_uct_device_context_t *dev_context, mca_btl_base_rdma_completion_fn_t cbfunc,
                                  void *cbcontext, void *cbdata)
{
    mca_btl_uct_uct_completion_t *comp = (mca_btl_uct_uct_completion_t *) opal_free_list_get (&dev_context->rdma_completions);
    if (OPAL_LIKELY(NULL != comp)) {
        comp->uct_comp.count = 1;
        comp->btl = &uct_btl->super;
        comp->endpoint = endpoint;
        comp->local_address = local_address;
        comp->local_handle = local_handle;
        comp->cbfunc = cbfunc;
        comp->cbcontext = cbcontext;
        comp->cbdata = cbdata;
        comp->dev_context = dev_context;
    }

    return comp;
}

void mca_btl_uct_uct_completion_release (mca_btl_uct_uct_completion_t *comp)
{
    if (comp) {
        opal_free_list_return (&comp->dev_context->rdma_completions, &comp->super);
    }
}

static void mca_btl_uct_get_unpack (void *arg, const void *data, size_t length)
{
    memcpy (arg, data, length);
}

int mca_btl_uct_get (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_rdma_context (uct_btl);
    mca_btl_uct_uct_completion_t *comp = NULL;
    ucs_status_t ucs_status;
    uct_rkey_bundle_t rkey;
    uct_ep_h ep_handle;
    int rc;

    BTL_VERBOSE(("performing get operation. local address: %p, length: %lu", local_address, (unsigned long) size));

    if (cbfunc) {
        comp = mca_btl_uct_uct_completion_alloc (uct_btl, endpoint, local_address, local_handle, context,
                                                 cbfunc, cbcontext, cbdata);
        if (OPAL_UNLIKELY(NULL == comp)) {
            BTL_VERBOSE(("culd not allocate completion structure"));
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    rc = mca_btl_uct_get_rkey (uct_btl, context, endpoint, remote_handle, &rkey, &ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_VERBOSE(("mca_btl_uct_get_rkey returned %d", rc));
        mca_btl_uct_uct_completion_release (comp);
        return rc;
    }

    mca_btl_uct_context_lock (context);

    if (size <= MCA_BTL_UCT_TL_ATTR(uct_btl->rdma_tl, context->context_id).cap.get.max_bcopy) {
        ucs_status = uct_ep_get_bcopy (ep_handle, mca_btl_uct_get_unpack, local_address, size, remote_address,
                                       rkey.rkey, &comp->uct_comp);
    } else {
        uct_iov_t iov = {.buffer = local_address, .length = size, .stride = 0, .count = 1,
                         .memh = MCA_BTL_UCT_REG_REMOTE_TO_LOCAL(local_handle)->uct_memh};
        ucs_status = uct_ep_get_zcopy (ep_handle, &iov, 1, remote_address, rkey.rkey, &comp->uct_comp);
    }

    /* go ahead and progress the worker while we have the lock (if we are not in an AM callback) */
    if (!context->in_am_callback) {
        (void) uct_worker_progress (context->uct_worker);
    }

    mca_btl_uct_context_unlock (context);

    if (!context->in_am_callback) {
        mca_btl_uct_device_handle_completions (context);
    }

    if (UCS_OK == ucs_status && cbfunc) {
        /* if UCS_OK is returned the callback will never fire so we have to make the callback
         * ourselves */
        cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);
    }

    if (UCS_INPROGRESS == ucs_status) {
        ucs_status = UCS_OK;
    } else {
        mca_btl_uct_uct_completion_release (comp);
    }

    BTL_VERBOSE(("get issued. status = %d", ucs_status));

    uct_rkey_release (&rkey);

    return OPAL_LIKELY(UCS_OK == ucs_status) ? OPAL_SUCCESS : OPAL_ERR_RESOURCE_BUSY;
}

struct mca_btl_uct_put_pack_args_t {
    void *local_address;
    size_t size;
};

typedef struct mca_btl_uct_put_pack_args_t mca_btl_uct_put_pack_args_t;

static size_t mca_btl_uct_put_pack (void *dest, void *arg)
{
    mca_btl_uct_put_pack_args_t *args = (mca_btl_uct_put_pack_args_t *) arg;

    memcpy (dest, args->local_address, args->size);
    return args->size;
}

int mca_btl_uct_put (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_rdma_context (uct_btl);
    mca_btl_uct_uct_completion_t *comp = NULL;
    ucs_status_t ucs_status;
    uct_rkey_bundle_t rkey;
    uct_ep_h ep_handle;
    bool use_short = false;
    bool use_bcopy = false;
    int rc;

    BTL_VERBOSE(("performing put operation. local address: %p, length: %lu", local_address, (unsigned long) size));

    if (size > uct_btl->super.btl_put_local_registration_threshold && cbfunc) {
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

    /* determine what UCT prototol should be used */
    if (size <= uct_btl->super.btl_put_local_registration_threshold) {
        use_short = size <= MCA_BTL_UCT_TL_ATTR(uct_btl->rdma_tl, context->context_id).cap.put.max_short;
        use_bcopy = !use_short;
    }

    do {
        if (use_short) {
            ucs_status = uct_ep_put_short (ep_handle, local_address, size, remote_address, rkey.rkey);
        } else if (use_bcopy) {
            ssize_t tmp = uct_ep_put_bcopy (ep_handle, mca_btl_uct_put_pack,
                                            &(mca_btl_uct_put_pack_args_t) {.local_address = local_address,
                                                    .size = size},
                                            remote_address, rkey.rkey);
            ucs_status = (tmp == (ssize_t) size) ? UCS_OK : UCS_ERR_NO_RESOURCE;
        } else {
            uct_iov_t iov = {.buffer = local_address, .length = size, .stride = 0, .count = 1,
                         .memh = MCA_BTL_UCT_REG_REMOTE_TO_LOCAL(local_handle)->uct_memh};

            ucs_status = uct_ep_put_zcopy (ep_handle, &iov, 1, remote_address, rkey.rkey, &comp->uct_comp);
        }

        /* go ahead and progress the worker while we have the lock */
        if (UCS_ERR_NO_RESOURCE != ucs_status || context->in_am_callback) {
            if (!context->in_am_callback) {
                (void) uct_worker_progress (context->uct_worker);
            }

            break;
        }

        /* wait for something to complete */
        while (!uct_worker_progress (context->uct_worker));
    } while (1);

    mca_btl_uct_context_unlock (context);

    mca_btl_uct_device_handle_completions (context);

    if (UCS_OK == ucs_status && cbfunc) {
        /* if UCS_OK is returned the callback will never fire so we have to make the callback
         * ourselves. this callback is possibly being made before the data is visible to the
         * remote process. */
        cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);
    }

    if (UCS_INPROGRESS == ucs_status) {
        ucs_status = UCS_OK;
    } else {
        mca_btl_uct_uct_completion_release (comp);
    }

    uct_rkey_release (&rkey);

    return OPAL_LIKELY(UCS_OK == ucs_status) ? OPAL_SUCCESS : OPAL_ERR_RESOURCE_BUSY;
}

int mca_btl_uct_flush (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    const int tl_index = uct_btl->rdma_tl->tl_index;
    const int context_count = mca_btl_uct_component.num_contexts_per_module;
    ucs_status_t ucs_status;

    BTL_VERBOSE(("mca_btl_uct_flush starting"));

    for (int i = 0 ; i < context_count ; ++i) {
        mca_btl_uct_device_context_t *context = uct_btl->rdma_tl->uct_dev_contexts[i];

        if (NULL == context) {
            continue;
        }

        mca_btl_uct_context_lock (context);
        /* this loop is here because at least some of the TLs do no support a
         * completion callback. its a real PIA but has to be done for now. */
        do {
            uct_worker_progress (context->uct_worker);

            if (NULL != endpoint && endpoint->uct_eps[context->context_id][tl_index].uct_ep) {
                ucs_status = uct_ep_flush (endpoint->uct_eps[context->context_id][tl_index].uct_ep, 0, NULL);
            } else {
                ucs_status = uct_iface_flush (context->uct_iface, 0, NULL);
            }
        } while (UCS_INPROGRESS == ucs_status);

        mca_btl_uct_context_unlock (context);
        mca_btl_uct_device_handle_completions (context);
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_flush_thread (mca_btl_base_module_t *btl)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    const int context_id = mca_btl_uct_get_context_index ();
    mca_btl_uct_device_context_t *context = uct_btl->rdma_tl->uct_dev_contexts[context_id];
    ucs_status_t ucs_status;

    BTL_VERBOSE(("mca_btl_uct_flush_thread starting"));

    if (NULL == context) {
        return OPAL_SUCCESS;
    }

    mca_btl_uct_context_lock (context);

    /* this loop is here because at least some of the TLs do no support a
     * completion callback. its a real PIA but has to be done for now. */
    do {
        uct_worker_progress (context->uct_worker);
        ucs_status = uct_iface_flush (context->uct_iface, 0, NULL);
    } while (UCS_INPROGRESS == ucs_status);

    mca_btl_uct_context_unlock (context);

    mca_btl_uct_device_handle_completions (context);

    return OPAL_SUCCESS;
}
