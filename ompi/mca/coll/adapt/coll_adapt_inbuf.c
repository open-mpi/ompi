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

#include "coll_adapt.h"
#include "coll_adapt_inbuf.h"

static void ompi_coll_adapt_inbuf_constructor(ompi_coll_adapt_inbuf_t * inbuf)
{
}

static void ompi_coll_adapt_inbuf_destructor(ompi_coll_adapt_inbuf_t * inbuf)
{
}

OBJ_CLASS_INSTANCE(ompi_coll_adapt_inbuf_t, opal_free_list_item_t, ompi_coll_adapt_inbuf_constructor,
                   ompi_coll_adapt_inbuf_destructor);
