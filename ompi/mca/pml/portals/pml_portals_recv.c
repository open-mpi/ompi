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

#include "ompi_config.h"
#include "pml_portals.h"
#include "ompi/request/request.h"

int mca_pml_portals_irecv_init( void *addr,
                            size_t count,
                            ompi_datatype_t * datatype,
                            int src,
                            int tag,
                            struct ompi_communicator_t *comm,
                            struct ompi_request_t **request )
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_irecv( void *addr,
                       size_t count,
                       ompi_datatype_t * datatype,
                       int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       struct ompi_request_t **request )
{
    return OMPI_SUCCESS;
}


int mca_pml_portals_recv( void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status )
{
    return OMPI_SUCCESS;
}

