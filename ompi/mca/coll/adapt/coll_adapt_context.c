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

static void mca_coll_adapt_bcast_context_constructor(mca_coll_adapt_bcast_context_t * bcast_context)
{
}

static void mca_coll_adapt_bcast_context_destructor(mca_coll_adapt_bcast_context_t * bcast_context)
{

}

static void
mca_coll_adapt_constant_bcast_context_constructor(mca_coll_adapt_constant_bcast_context_t * con)
{
}

static void mca_coll_adapt_constant_bcast_context_destructor(mca_coll_adapt_constant_bcast_context_t
                                                             * con)
{
}


OBJ_CLASS_INSTANCE(mca_coll_adapt_bcast_context_t, opal_free_list_item_t,
                   mca_coll_adapt_bcast_context_constructor,
                   mca_coll_adapt_bcast_context_destructor);

OBJ_CLASS_INSTANCE(mca_coll_adapt_constant_bcast_context_t, opal_object_t,
                   mca_coll_adapt_constant_bcast_context_constructor,
                   mca_coll_adapt_constant_bcast_context_destructor);

static void mca_coll_adapt_reduce_context_constructor(mca_coll_adapt_reduce_context_t *
                                                      reduce_context)
{
}

static void mca_coll_adapt_reduce_context_destructor(mca_coll_adapt_reduce_context_t *
                                                     reduce_context)
{

}

static void
mca_coll_adapt_constant_reduce_context_constructor(mca_coll_adapt_constant_reduce_context_t * con)
{
}

static void
mca_coll_adapt_constant_reduce_context_destructor(mca_coll_adapt_constant_reduce_context_t * con)
{
}


OBJ_CLASS_INSTANCE(mca_coll_adapt_reduce_context_t, opal_free_list_item_t,
                   mca_coll_adapt_reduce_context_constructor,
                   mca_coll_adapt_reduce_context_destructor);

OBJ_CLASS_INSTANCE(mca_coll_adapt_constant_reduce_context_t, opal_object_t,
                   mca_coll_adapt_constant_reduce_context_constructor,
                   mca_coll_adapt_constant_reduce_context_destructor);
