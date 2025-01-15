/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_UCT_ENDPOINT_H
#define MCA_BTL_UCT_ENDPOINT_H

#include "btl_uct.h"
#include "opal/class/opal_list.h"
#include "opal/util/event.h"

BEGIN_C_DECLS

mca_btl_base_endpoint_t *mca_btl_uct_endpoint_create(opal_proc_t *proc);
int mca_btl_uct_endpoint_connect(mca_btl_uct_module_t *module, mca_btl_uct_endpoint_t *endpoint,
                                 int ep_index, void *ep_addr, int tl_index);

static inline bool mca_btl_uct_tl_endpoint_ready(mca_btl_uct_tl_endpoint_t *tl_endpoint)
{
    return !!(tl_endpoint->flags & MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY);
}

/**
 * @brief Mark all frags associated with the endpoint/context_id pair as ready.
 *
 * @param[in] module      UCT BTL module
 * @param[in] endpoint    UCT BTL endpoint
 * @param[in] context_id  context id
 *
 * Requires holding the endpoint mutex.
 */
static inline void btl_uct_release_pending_frags(mca_btl_uct_module_t *module, mca_btl_base_endpoint_t *endpoint,
                                                 int context_id)
{
    mca_btl_uct_base_frag_t *frag;
    OPAL_LIST_FOREACH (frag, &module->pending_frags, mca_btl_uct_base_frag_t) {
        if (frag->context->context_id == context_id && endpoint == frag->endpoint) {
            frag->ready = true;
        }
    }
}

static inline int mca_btl_uct_endpoint_test_am(mca_btl_uct_module_t *module,
                                               mca_btl_uct_endpoint_t *endpoint,
                                               mca_btl_uct_device_context_t *context,
                                               uct_ep_h *ep_handle)
{
    int tl_index = module->am_tl->tl_index;
    int ep_index = context->context_id;

    if (OPAL_LIKELY(mca_btl_uct_tl_endpoint_ready(endpoint->uct_eps[ep_index] + tl_index))) {
        *ep_handle = endpoint->uct_eps[ep_index][tl_index].uct_ep;
        return OPAL_SUCCESS;
    }

    return OPAL_ERR_NOT_AVAILABLE;
}

static inline void mca_btl_uct_tl_endpoint_set_flag(mca_btl_uct_tl_endpoint_t *tl_endpoint, int32_t flag)
{
    int32_t flags = opal_atomic_or_fetch_32(&tl_endpoint->flags, flag);
    int32_t conn_ready_flags = MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REM_READY | MCA_BTL_UCT_ENDPOINT_FLAG_CONN_REC;
    if ((flags & conn_ready_flags) == conn_ready_flags) {
        /* remote side is ready and the local endpoint is connected */
        (void) opal_atomic_or_fetch_32(&tl_endpoint->flags, MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY);
    }
}

/**
 * @brief Check if the endpoint is connected and start the connection if not
 *
 * @param[in] module      UCT BTL module
 * @param[in] endpoint    UCT BTL endpoint
 * @param[in] context     UCT BTL device context
 * @param[out] ep_handle  UCT endpoint handle
 * @param[in] tl_index    UCT TL index (0 or 1)
 *
 * @returns OPAL_SUCCESS if the endpoint is connected and ready to us
 * @returns OPAL_ERR_RESOURCE_BUSY if the connection is underway
 * @returns OPAL_ERROR otherwise
 */
static inline int mca_btl_uct_endpoint_check(mca_btl_uct_module_t *module,
                                             mca_btl_uct_endpoint_t *endpoint,
                                             mca_btl_uct_device_context_t *context,
                                             uct_ep_h *ep_handle, const int tl_index)
{
    int ep_index = context->context_id;
    int rc;

    if (OPAL_LIKELY(mca_btl_uct_tl_endpoint_ready(endpoint->uct_eps[ep_index] + tl_index))) {
        *ep_handle = endpoint->uct_eps[ep_index][tl_index].uct_ep;
        return OPAL_SUCCESS;
    }

    rc = mca_btl_uct_endpoint_connect(module, endpoint, ep_index, NULL, tl_index);
    *ep_handle = endpoint->uct_eps[ep_index][tl_index].uct_ep;
    BTL_VERBOSE(
        ("mca_btl_uct_endpoint_connect returned %d. context id = %d, flags = 0x%x", rc, ep_index,
         MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY & endpoint->uct_eps[ep_index][tl_index].flags));
    return rc;
}

static inline int mca_btl_uct_endpoint_check_rdma(mca_btl_uct_module_t *module,
                                                  mca_btl_uct_endpoint_t *endpoint,
                                                  mca_btl_uct_device_context_t *context,
                                                  uct_ep_h *ep_handle)
{
    assert(NULL != module->rdma_tl);
    return mca_btl_uct_endpoint_check(module, endpoint, context, ep_handle,
                                      module->rdma_tl->tl_index);
}

static inline int mca_btl_uct_endpoint_check_am(mca_btl_uct_module_t *module,
                                                mca_btl_uct_endpoint_t *endpoint,
                                                mca_btl_uct_device_context_t *context,
                                                uct_ep_h *ep_handle)
{
    assert(NULL != module->am_tl);
    return mca_btl_uct_endpoint_check(module, endpoint, context, ep_handle,
                                      module->am_tl->tl_index);
}

END_C_DECLS
#endif
