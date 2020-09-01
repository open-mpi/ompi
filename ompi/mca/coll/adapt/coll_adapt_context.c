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


OBJ_CLASS_INSTANCE(ompi_coll_adapt_bcast_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_bcast_context_t, opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_reduce_context_t, opal_free_list_item_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_coll_adapt_constant_reduce_context_t, opal_object_t,
                   NULL, NULL);
