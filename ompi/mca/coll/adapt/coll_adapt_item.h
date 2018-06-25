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

#include "opal/class/opal_list.h"
#include "coll_adapt_inbuf.h"

struct mca_coll_adapt_item_s {
    opal_list_item_t super;
    /* Fragment id */
    int id;
    /* The number of children which have received the current segment from */
    int count;
};

typedef struct mca_coll_adapt_item_s mca_coll_adapt_item_t;

OBJ_CLASS_DECLARATION(mca_coll_adapt_item_t);
