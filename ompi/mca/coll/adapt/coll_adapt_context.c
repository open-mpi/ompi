/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA CORPORATION. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi/mca/coll/coll.h"
#include "coll_adapt_context.h"


static void adapt_constant_context_construct(ompi_coll_adapt_constant_context_t *context)
{
    OBJ_CONSTRUCT(&context->cb_deferred, opal_list_t);
    OBJ_CONSTRUCT(&context->cb_mutex, opal_mutex_t);
    context->cb_running = false;
}

static void adapt_constant_context_destruct(ompi_coll_adapt_constant_context_t *context)
{
    OBJ_DESTRUCT(&context->cb_mutex);
    OBJ_DESTRUCT(&context->cb_deferred);
}

static void adapt_constant_reduce_context_construct(ompi_coll_adapt_constant_reduce_context_t *context)
{
    OBJ_CONSTRUCT(&context->recv_list, opal_list_t);
    OBJ_CONSTRUCT(&context->mutex_recv_list, opal_mutex_t);
    OBJ_CONSTRUCT(&context->inbuf_list, opal_free_list_t);
}

static void adapt_constant_reduce_context_destruct(ompi_coll_adapt_constant_reduce_context_t *context)
{
    OBJ_DESTRUCT(&context->mutex_recv_list);
    OBJ_DESTRUCT(&context->recv_list);
    OBJ_DESTRUCT(&context->inbuf_list);
}

void ompi_coll_adapt_run_callbacks(ompi_coll_adapt_constant_context_t * con,
                                   ompi_coll_adapt_context_t * context)
{
    OPAL_THREAD_LOCK(&con->cb_mutex);
    if (con->cb_running) {
        opal_list_append(&con->cb_deferred, (opal_list_item_t *) context);
        OPAL_THREAD_UNLOCK(&con->cb_mutex);
        return;
    }
    con->cb_running = true;
    OPAL_THREAD_UNLOCK(&con->cb_mutex);

    /* A body can finish the collective; hold a reference so the list and the
     * lock below outlive it. */
    OBJ_RETAIN(con);

    do {
        (void) context->cb_body(context);

        OPAL_THREAD_LOCK(&con->cb_mutex);
        context = (ompi_coll_adapt_context_t *) opal_list_remove_first(&con->cb_deferred);
        if (NULL == context) {
            con->cb_running = false;
        }
        OPAL_THREAD_UNLOCK(&con->cb_mutex);
    } while (NULL != context);

    OBJ_RELEASE(con);
}

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_context_t, opal_object_t,
                   &adapt_constant_context_construct,
                   &adapt_constant_context_destruct);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_bcast_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_bcast_context_t, ompi_coll_adapt_constant_context_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_reduce_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_reduce_context_t, ompi_coll_adapt_constant_context_t,
                   &adapt_constant_reduce_context_construct,
                   &adapt_constant_reduce_context_destruct);
