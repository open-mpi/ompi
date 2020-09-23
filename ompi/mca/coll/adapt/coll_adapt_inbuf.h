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

#ifndef MCA_COLL_ADAPT_INBUF_H
#define MCA_COLL_ADAPT_INBUF_H

#include "opal/class/opal_free_list.h"

struct ompi_coll_adapt_inbuf_s {
    opal_free_list_item_t super;
    char buff[];
};

typedef struct ompi_coll_adapt_inbuf_s ompi_coll_adapt_inbuf_t;

OBJ_CLASS_DECLARATION(ompi_coll_adapt_inbuf_t);

#endif                          /* MCA_COLL_ADAPT_INBUF_H */
