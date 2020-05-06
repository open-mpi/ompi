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

#include "coll_adapt_item.h"

static void ompi_coll_adapt_item_constructor(ompi_coll_adapt_item_t * item)
{
}

static void ompi_coll_adapt_item_destructor(ompi_coll_adapt_item_t * item)
{
}

OBJ_CLASS_INSTANCE(ompi_coll_adapt_item_t, opal_list_item_t, ompi_coll_adapt_item_constructor,
                   ompi_coll_adapt_item_destructor);
