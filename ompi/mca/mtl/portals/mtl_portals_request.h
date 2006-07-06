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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_REQUEST_H
#define OMPI_MTL_PORTALS_REQUEST_H

struct ompi_mtl_portals_request_t {
    struct mca_mtl_request_t super;
    bool free_after;
    struct ompi_convertor_t *convertor;

    int (*event_callback)(ptl_event_t *ev, struct ompi_mtl_portals_request_t*);
};
typedef struct ompi_mtl_portals_request_t ompi_mtl_portals_request_t;


#endif
