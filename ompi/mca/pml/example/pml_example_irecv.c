/*
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "pml_example.h"
#include "request/request.h"

int mca_pml_example_irecv_init( void *addr,
                            size_t count,
                            ompi_datatype_t * datatype,
                            int src,
                            int tag,
                            struct ompi_communicator_t *comm,
                            struct ompi_request_t **request )
{
    return OMPI_SUCCESS;
}

int mca_pml_example_irecv( void *addr,
                       size_t count,
                       ompi_datatype_t * datatype,
                       int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       struct ompi_request_t **request )
{
    return OMPI_SUCCESS;
}


int mca_pml_example_recv( void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status )
{
    return OMPI_SUCCESS;
}

