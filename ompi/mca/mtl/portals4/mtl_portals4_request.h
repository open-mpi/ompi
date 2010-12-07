/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_REQUEST_H
#define OMPI_MTL_PORTALS_REQUEST_H

#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/mtl.h"

struct ompi_mtl_portals4_request_t {
    struct mca_mtl_request_t super;
    int (*event_callback)(ptl_event_t *ev, struct ompi_mtl_portals4_request_t*);
    void *buffer_ptr; /* send and receive side */
    ptl_handle_md_t md_h; /* send and receive side */
    ptl_handle_me_t me_h; /* send and receive side */
    int event_count; /* send side */
    struct opal_convertor_t *convertor; /* recv side */
    void *delivery_ptr; /* recv side */
    size_t delivery_len; /* recv side */
};
typedef struct ompi_mtl_portals4_request_t ompi_mtl_portals4_request_t;


#endif
