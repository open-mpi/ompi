/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdbool.h>
#include <stdlib.h>
#include <uct/api/uct.h>
#include <uct/api/uct_def.h>

#include "btl_uct.h"
#include "btl_uct_device_context.h"
#include "btl_uct_types.h"

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_object.h"

#if HAVE_DECL_UCT_CB_FLAG_SYNC
#    define MCA_BTL_UCT_CB_FLAG_SYNC UCT_CB_FLAG_SYNC
#else
#    define MCA_BTL_UCT_CB_FLAG_SYNC 0
#endif

static void mca_btl_uct_context_enable_progress(mca_btl_uct_device_context_t *context)
{
    if (!context->progress_enabled) {
#if HAVE_DECL_UCT_PROGRESS_THREAD_SAFE
        uct_iface_progress_enable(context->uct_iface,
                                  UCT_PROGRESS_THREAD_SAFE | UCT_PROGRESS_SEND | UCT_PROGRESS_RECV);
#else
        uct_iface_progress_enable(context->uct_iface, UCT_PROGRESS_SEND | UCT_PROGRESS_RECV);
#endif
        context->progress_enabled = true;
    }
}

void mca_btl_uct_context_enable_am_handler(mca_btl_uct_tl_t *tl,
                                           mca_btl_uct_device_context_t *context)
{
    if (context->am_handler_installed) {
        return;
    }

    BTL_VERBOSE(("installing AM handler for tl %s::%s context id %d",
                 tl->uct_md->md_name, tl->uct_tl_name, context->context_id));
    uct_iface_set_am_handler(context->uct_iface, MCA_BTL_UCT_FRAG, mca_btl_uct_am_handler,
                             context, MCA_BTL_UCT_CB_FLAG_SYNC);
    context->am_handler_installed = true;
}

mca_btl_uct_device_context_t *mca_btl_uct_context_create(mca_btl_uct_module_t *module,
                                                         mca_btl_uct_tl_t *tl, int context_id,
                                                         bool enable_progress)
{
#if UCT_API >= UCT_VERSION(1, 6)
    uct_iface_params_t iface_params = {.field_mask = UCT_IFACE_PARAM_FIELD_OPEN_MODE
                                                     | UCT_IFACE_PARAM_FIELD_DEVICE,
                                       .open_mode = UCT_IFACE_OPEN_MODE_DEVICE,
                                       .mode = {.device = {.tl_name = tl->uct_tl_name,
                                                           .dev_name = tl->uct_dev_name}}};
#else
    uct_iface_params_t iface_params = {.rndv_cb = NULL,
                                       .eager_cb = NULL,
                                       .stats_root = NULL,
                                       .rx_headroom = 0,
                                       .open_mode = UCT_IFACE_OPEN_MODE_DEVICE,
                                       .mode = {.device = {.tl_name = tl->uct_tl_name,
                                                           .dev_name = tl->uct_dev_name}}};
#endif
    mca_btl_uct_device_context_t *context;
    ucs_status_t ucs_status;
    int rc;

    context = calloc(1, sizeof(*context));
    if (OPAL_UNLIKELY(NULL == context)) {
        return NULL;
    }

    context->context_id = context_id;
    context->uct_btl = module;
    OBJ_CONSTRUCT(&context->completion_fifo, opal_fifo_t);
    OBJ_CONSTRUCT(&context->mutex, opal_recursive_mutex_t);
    OBJ_CONSTRUCT(&context->rdma_completions, opal_free_list_t);

    rc = opal_free_list_init(&context->rdma_completions, sizeof(mca_btl_uct_uct_completion_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_uct_uct_completion_t), 0,
                             opal_cache_line_size, 0, 4096, 128, NULL, 0, NULL, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        mca_btl_uct_context_destroy(context);
        return NULL;
    }

    /* apparently (in contradiction to the spec) UCT is *not* thread safe. because we have to
     * use our own locks just go ahead and use UCS_THREAD_MODE_SINGLE. if they ever fix their
     * api then change this back to UCS_THREAD_MODE_MULTI and remove the locks around the
     * various UCT calls. */
    ucs_status = uct_worker_create(tl->ucs_async, UCS_THREAD_MODE_SINGLE, &context->uct_worker);
    if (OPAL_UNLIKELY(UCS_OK != ucs_status)) {
        BTL_VERBOSE(("could not create a UCT worker"));
        mca_btl_uct_context_destroy(context);
        return NULL;
    }

    ucs_status = uct_iface_open(tl->uct_md->uct_md, context->uct_worker, &iface_params,
                                tl->uct_tl_config, &context->uct_iface);
    if (OPAL_UNLIKELY(UCS_OK != ucs_status)) {
        BTL_VERBOSE(("could not open UCT interface. error code: %d", ucs_status));
        mca_btl_uct_context_destroy(context);
        return NULL;
    }

    if (module != NULL && tl == module->am_tl) {
        mca_btl_uct_context_enable_am_handler(tl, context);
    }

    if (enable_progress) {
        BTL_VERBOSE(("enabling progress for tl %s::%s context id %d",
                     tl->uct_md->md_name, tl->uct_tl_name, context_id));
        mca_btl_uct_context_enable_progress(context);
    }

    return context;
}

void mca_btl_uct_context_destroy(mca_btl_uct_device_context_t *context)
{
    if (context->uct_iface) {
        uct_iface_close(context->uct_iface);
        context->uct_iface = NULL;
    }

    if (context->uct_worker) {
        uct_worker_destroy(context->uct_worker);
        context->uct_worker = NULL;
    }

    OBJ_DESTRUCT(&context->completion_fifo);
    OBJ_DESTRUCT(&context->rdma_completions);
    free(context);
}

