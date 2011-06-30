/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OSC_PT2PT_BUFFER_H
#define OMPI_OSC_PT2PT_BUFFER_H

#include "opal/class/opal_free_list.h"
#include "ompi/request/request.h"

BEGIN_C_DECLS

struct ompi_osc_pt2pt_buffer_t {
    ompi_free_list_item_t super;

    ompi_request_t *request;
    void *data;
    void *payload;
    size_t len;
};
typedef struct ompi_osc_pt2pt_buffer_t ompi_osc_pt2pt_buffer_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_buffer_t);

END_C_DECLS

#endif
