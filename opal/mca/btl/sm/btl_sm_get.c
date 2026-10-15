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
#include "opal/mca/threads/mutex.h"

/* Accelerator gets are queued on one of a few streams (chosen by peer, so copies from different
 * peers overlap) and completed from progress once their event fires. */
#define MCA_BTL_SM_GET_STREAMS 8
#define MCA_BTL_SM_MAX_ASYNC_GETS 64

struct mca_btl_sm_async_get_t {
    opal_accelerator_event_t *event;
    mca_btl_base_module_t *btl;
    mca_btl_base_endpoint_t *endpoint;
    void *local_address;
    mca_btl_base_registration_handle_t *local_handle;
    mca_btl_base_rdma_completion_fn_t cbfunc;
    void *cbcontext;
    void *cbdata;
};
typedef struct mca_btl_sm_async_get_t mca_btl_sm_async_get_t;

static opal_mutex_t async_get_lock = OPAL_MUTEX_STATIC_INIT;
static opal_accelerator_stream_t *get_streams[MCA_BTL_SM_GET_STREAMS];
static mca_btl_sm_async_get_t async_gets[MCA_BTL_SM_MAX_ASYNC_GETS];
static int num_async_gets = 0;
/* events of completed gets, reused by later ones */
static opal_accelerator_event_t *free_events[MCA_BTL_SM_MAX_ASYNC_GETS];
static int num_free_events = 0;
static bool async_gets_disabled = false;

#define MCA_BTL_SM_GET_SYNC 2

/* Queue the copy on the peer's stream. Returns 1 when queued, 0 when the copy was done
 * synchronously, MCA_BTL_SM_GET_SYNC when the caller should do a plain synchronous get, or an
 * OPAL error code. */
static int mca_btl_sm_get_async(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                                void *local_address, uint64_t remote_address,
                                mca_btl_base_registration_handle_t *local_handle,
                                mca_btl_base_registration_handle_t *remote_handle, size_t size,
                                mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                                void *cbdata)
{
    int slot = endpoint->peer_smp_rank % MCA_BTL_SM_GET_STREAMS;
    opal_accelerator_event_t *event = NULL;
    mca_btl_sm_async_get_t *get;
    int ret;

    OPAL_THREAD_LOCK(&async_get_lock);
    if (async_gets_disabled || MCA_BTL_SM_MAX_ASYNC_GETS == num_async_gets) {
        OPAL_THREAD_UNLOCK(&async_get_lock);
        return MCA_BTL_SM_GET_SYNC;
    }

    ret = mca_smsc->copy_from_async(endpoint->smsc_endpoint, local_address,
                                    (void *) (intptr_t) remote_address, size, remote_handle,
                                    &get_streams[slot]);
    if (1 != ret) {
        if (NULL == get_streams[slot] && 0 > ret) {
            /* no stream could be created: stop trying */
            async_gets_disabled = true;
            ret = MCA_BTL_SM_GET_SYNC;
        }
        OPAL_THREAD_UNLOCK(&async_get_lock);
        return ret;
    }

    if (0 < num_free_events) {
        event = free_events[--num_free_events];
    } else if (OPAL_SUCCESS != opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &event, false)) {
        event = NULL;
    }
    if (NULL == event
        || OPAL_SUCCESS != opal_accelerator.record_event(MCA_ACCELERATOR_NO_DEVICE_ID, event, get_streams[slot])) {
        /* the copy is queued but cannot be tracked: wait for it here */
        if (NULL != event) {
            free_events[num_free_events++] = event;
        }
        ret = opal_accelerator.sync_stream(get_streams[slot]);
        OPAL_THREAD_UNLOCK(&async_get_lock);
        return (OPAL_SUCCESS == ret) ? 0 : ret;
    }

    get = &async_gets[num_async_gets++];
    get->event = event;
    get->btl = btl;
    get->endpoint = endpoint;
    get->local_address = local_address;
    get->local_handle = local_handle;
    get->cbfunc = cbfunc;
    get->cbcontext = cbcontext;
    get->cbdata = cbdata;
    OPAL_THREAD_UNLOCK(&async_get_lock);

    return 1;
}

int mca_btl_sm_progress_async_gets(void)
{
    mca_btl_sm_async_get_t done;
    int count = 0, ret;

    if (0 == num_async_gets) {
        return 0;
    }

    OPAL_THREAD_LOCK(&async_get_lock);
    for (int i = 0; i < num_async_gets;) {
        ret = opal_accelerator.query_event(MCA_ACCELERATOR_NO_DEVICE_ID, async_gets[i].event);
        if (OPAL_ERR_RESOURCE_BUSY == ret) {
            ++i;
            continue;
        }
        done = async_gets[i];
        async_gets[i] = async_gets[--num_async_gets];
        free_events[num_free_events++] = done.event;
        /* the callback may start another get */
        OPAL_THREAD_UNLOCK(&async_get_lock);
        done.cbfunc(done.btl, done.endpoint, done.local_address, done.local_handle,
                    done.cbcontext, done.cbdata, (OPAL_SUCCESS == ret) ? OPAL_SUCCESS : OPAL_ERROR);
        ++count;
        OPAL_THREAD_LOCK(&async_get_lock);
    }
    OPAL_THREAD_UNLOCK(&async_get_lock);

    return count;
}

void mca_btl_sm_async_gets_fini(void)
{
    for (int i = 0; i < MCA_BTL_SM_GET_STREAMS; ++i) {
        if (NULL != get_streams[i]) {
            OBJ_RELEASE(get_streams[i]);
            get_streams[i] = NULL;
        }
    }
    for (int i = 0; i < num_async_gets; ++i) {
        OBJ_RELEASE(async_gets[i].event);
    }
    num_async_gets = 0;
    for (int i = 0; i < num_free_events; ++i) {
        OBJ_RELEASE(free_events[i]);
    }
    num_free_events = 0;
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
        int ret = mca_btl_sm_get_async(btl, endpoint, local_address, remote_address, local_handle,
                                       remote_handle, size, cbfunc, cbcontext, cbdata);
        if (1 == ret) {
            return OPAL_SUCCESS;
        }
        if (OPAL_UNLIKELY(0 > ret)) {
            return ret;
        }
        if (0 == ret) {
            cbfunc(btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);
            return OPAL_SUCCESS;
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
