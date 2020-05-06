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

static void ompi_coll_adapt_bcast_context_constructor(ompi_coll_adapt_bcast_context_t * bcast_context)
{
}

static void ompi_coll_adapt_bcast_context_destructor(ompi_coll_adapt_bcast_context_t * bcast_context)
{
}

static void
ompi_coll_adapt_constant_bcast_context_constructor(ompi_coll_adapt_constant_bcast_context_t * con)
{
}

static void ompi_coll_adapt_constant_bcast_context_destructor(ompi_coll_adapt_constant_bcast_context_t
                                                             * con)
{
}


OBJ_CLASS_INSTANCE(ompi_coll_adapt_bcast_context_t, opal_free_list_item_t,
                   ompi_coll_adapt_bcast_context_constructor,
                   ompi_coll_adapt_bcast_context_destructor);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_bcast_context_t, opal_object_t,
                   ompi_coll_adapt_constant_bcast_context_constructor,
                   ompi_coll_adapt_constant_bcast_context_destructor);

static void ompi_coll_adapt_reduce_context_constructor(ompi_coll_adapt_reduce_context_t *
                                                      reduce_context)
{
}

static void ompi_coll_adapt_reduce_context_destructor(ompi_coll_adapt_reduce_context_t *
                                                     reduce_context)
{
}

static void
ompi_coll_adapt_constant_reduce_context_constructor(ompi_coll_adapt_constant_reduce_context_t * con)
{
}

static void
ompi_coll_adapt_constant_reduce_context_destructor(ompi_coll_adapt_constant_reduce_context_t * con)
{
}


OBJ_CLASS_INSTANCE(ompi_coll_adapt_reduce_context_t, opal_free_list_item_t,
                   ompi_coll_adapt_reduce_context_constructor,
                   ompi_coll_adapt_reduce_context_destructor);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_reduce_context_t, opal_object_t,
                   ompi_coll_adapt_constant_reduce_context_constructor,
                   ompi_coll_adapt_constant_reduce_context_destructor);
