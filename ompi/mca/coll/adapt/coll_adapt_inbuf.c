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

static void mca_coll_adapt_inbuf_constructor(mca_coll_adapt_inbuf_t * inbuf)
{
}

static void mca_coll_adapt_inbuf_destructor(mca_coll_adapt_inbuf_t * inbuf)
{
}

OBJ_CLASS_INSTANCE(mca_coll_adapt_inbuf_t, opal_free_list_item_t, mca_coll_adapt_inbuf_constructor,
                   mca_coll_adapt_inbuf_destructor);
