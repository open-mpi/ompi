/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/coll/coll.h"
#include "coll_adapt_context.h"


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


OBJ_CLASS_INSTANCE(ompi_coll_adapt_bcast_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_bcast_context_t, opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_reduce_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_reduce_context_t, opal_object_t,
                   &adapt_constant_reduce_context_construct,
                   &adapt_constant_reduce_context_destruct);
