/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2019-2022 Google, Inc. All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/btl/sm/btl_sm.h"
#include "opal/mca/btl/sm/btl_sm_frag.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/mca/threads/mutex.h"

/* A get from a peer's device buffer is queued on the module's accelerator stream, with an event
 * recorded after it, and its callback runs from progress once the event has fired. The stream is
 * created on the first such get. */
struct mca_btl_sm_async_get_t {
    opal_free_list_item_t super;
    /** recorded after the copy; created with the first get that uses this object, and kept */
    opal_accelerator_event_t *event;
    int status;
    mca_btl_base_endpoint_t *endpoint;
    void *local_address;
    mca_btl_base_registration_handle_t *local_handle;
    mca_btl_base_rdma_completion_fn_t cbfunc;
    void *cbcontext;
    void *cbdata;
};
typedef struct mca_btl_sm_async_get_t mca_btl_sm_async_get_t;

static void mca_btl_sm_async_get_construct(mca_btl_sm_async_get_t *get)
{
    get->event = NULL;
}

static void mca_btl_sm_async_get_destruct(mca_btl_sm_async_get_t *get)
{
    if (NULL != get->event) {
        OBJ_RELEASE(get->event);
    }
}

static OBJ_CLASS_INSTANCE(mca_btl_sm_async_get_t, opal_free_list_item_t,
                          mca_btl_sm_async_get_construct, mca_btl_sm_async_get_destruct);

int mca_btl_sm_async_gets_init(mca_btl_sm_t *sm_btl)
{
    OBJ_CONSTRUCT(&sm_btl->async_get_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&sm_btl->async_gets, opal_list_t);
    OBJ_CONSTRUCT(&sm_btl->async_get_free, opal_free_list_t);
    sm_btl->get_stream = NULL;
    sm_btl->async_gets_disabled = false;

    return opal_free_list_init(&sm_btl->async_get_free, sizeof(mca_btl_sm_async_get_t),
                               opal_cache_line_size, OBJ_CLASS(mca_btl_sm_async_get_t), 0,
                               opal_cache_line_size, 0, -1, 8, NULL, 0, NULL, NULL, NULL);
}

void mca_btl_sm_async_gets_fini(mca_btl_sm_t *sm_btl)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&sm_btl->async_gets))) {
        opal_free_list_return(&sm_btl->async_get_free, (opal_free_list_item_t *) item);
    }
    OBJ_DESTRUCT(&sm_btl->async_gets);
    OBJ_DESTRUCT(&sm_btl->async_get_free);
    if (NULL != sm_btl->get_stream) {
        OBJ_RELEASE(sm_btl->get_stream);
        sm_btl->get_stream = NULL;
    }
    OBJ_DESTRUCT(&sm_btl->async_get_lock);
}

/* Queue a get on the module's stream. Returns OPAL_SUCCESS when the get was started (its callback
 * runs from progress) or done, OPAL_ERR_NOT_AVAILABLE when it cannot be queued and the caller has
 * to do it synchronously, or another OPAL error code. */
static int mca_btl_sm_get_async(mca_btl_sm_t *sm_btl, mca_btl_base_endpoint_t *endpoint,
                                void *local_address, uint64_t remote_address,
                                mca_btl_base_registration_handle_t *local_handle,
                                mca_btl_base_registration_handle_t *remote_handle, size_t size,
                                mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                                void *cbdata)
{
    mca_btl_sm_async_get_t *get;
    int ret;

    if (sm_btl->async_gets_disabled) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    OPAL_THREAD_LOCK(&sm_btl->async_get_lock);
    ret = mca_smsc->copy_from_async(endpoint->smsc_endpoint, local_address,
                                    (void *) (intptr_t) remote_address, size, remote_handle,
                                    &sm_btl->get_stream);
    if (OPAL_SUCCESS != ret) {
        if (OPAL_ERR_NOT_AVAILABLE != ret && NULL == sm_btl->get_stream) {
            /* no stream could be created: stop trying */
            sm_btl->async_gets_disabled = true;
            ret = OPAL_ERR_NOT_AVAILABLE;
        }
        OPAL_THREAD_UNLOCK(&sm_btl->async_get_lock);
        return ret;
    }

    get = (mca_btl_sm_async_get_t *) opal_free_list_get(&sm_btl->async_get_free);
    if (NULL != get && NULL == get->event
        && OPAL_SUCCESS != opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &get->event,
                                                         false)) {
        get->event = NULL;
    }
    if (NULL == get || NULL == get->event
        || OPAL_SUCCESS != opal_accelerator.record_event(MCA_ACCELERATOR_NO_DEVICE_ID, get->event,
                                                         sm_btl->get_stream)) {
        /* the copy is queued but cannot be tracked: wait for it here */
        if (NULL != get) {
            opal_free_list_return(&sm_btl->async_get_free, &get->super);
        }
        ret = opal_accelerator.sync_stream(sm_btl->get_stream);
        OPAL_THREAD_UNLOCK(&sm_btl->async_get_lock);
        if (OPAL_SUCCESS == ret) {
            cbfunc(&sm_btl->super, endpoint, local_address, local_handle, cbcontext, cbdata,
                   OPAL_SUCCESS);
        }
        return ret;
    }

    get->endpoint = endpoint;
    get->local_address = local_address;
    get->local_handle = local_handle;
    get->cbfunc = cbfunc;
    get->cbcontext = cbcontext;
    get->cbdata = cbdata;
    opal_list_append(&sm_btl->async_gets, &get->super.super);
    OPAL_THREAD_UNLOCK(&sm_btl->async_get_lock);

    return OPAL_SUCCESS;
}

int mca_btl_sm_progress_async_gets(mca_btl_sm_t *sm_btl)
{
    mca_btl_sm_async_get_t *get, *next;
    opal_list_t done;
    int count = 0;

    if (opal_list_is_empty(&sm_btl->async_gets)) {
        return 0;
    }

    OBJ_CONSTRUCT(&done, opal_list_t);
    OPAL_THREAD_LOCK(&sm_btl->async_get_lock);
    /* the gets share one stream, so they finish in the order they were queued */
    OPAL_LIST_FOREACH_SAFE (get, next, &sm_btl->async_gets, mca_btl_sm_async_get_t) {
        get->status = opal_accelerator.query_event(MCA_ACCELERATOR_NO_DEVICE_ID, get->event);
        if (OPAL_ERR_RESOURCE_BUSY == get->status) {
            break;
        }
        opal_list_remove_item(&sm_btl->async_gets, &get->super.super);
        opal_list_append(&done, &get->super.super);
    }
    OPAL_THREAD_UNLOCK(&sm_btl->async_get_lock);

    /* outside the lock: a callback may start another get */
    while (NULL != (get = (mca_btl_sm_async_get_t *) opal_list_remove_first(&done))) {
        get->cbfunc(&sm_btl->super, get->endpoint, get->local_address, get->local_handle,
                    get->cbcontext, get->cbdata,
                    (OPAL_SUCCESS == get->status) ? OPAL_SUCCESS : OPAL_ERROR);
        opal_free_list_return(&sm_btl->async_get_free, &get->super);
        ++count;
    }
    OBJ_DESTRUCT(&done);

    return count;
}

/**
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_sm_get(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                   void *local_address, uint64_t remote_address,
                   mca_btl_base_registration_handle_t *local_handle,
                   mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                   int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                   void *cbdata)
{
    if (!mca_btl_is_self_endpoint(endpoint) && NULL != mca_smsc->copy_from_async) {
        int ret = mca_btl_sm_get_async((mca_btl_sm_t *) btl, endpoint, local_address,
                                       remote_address, local_handle, remote_handle, size, cbfunc,
                                       cbcontext, cbdata);
        if (OPAL_ERR_NOT_AVAILABLE != ret) {
            return ret;
        }
    }

    if (!mca_btl_is_self_endpoint(endpoint)) {
        int ret = MCA_SMSC_CALL(copy_from, endpoint->smsc_endpoint, local_address,
                                (void *) (intptr_t) remote_address, size, remote_handle);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            return ret;
        }
    } else {
        memcpy(local_address, (void *)(uintptr_t) remote_address, size);
    }

    /* always call the callback function */
    cbfunc(btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}
