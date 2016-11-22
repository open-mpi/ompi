/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "pml_example.h"
#include "ompi/request/request.h"

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

int mca_pml_example_imrecv(void *buf,
                           size_t count,
                           ompi_datatype_t *datatype,
                           struct ompi_message_t **message,
                           struct ompi_request_t **request)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_mrecv(void *buf,
                          size_t count,
                          ompi_datatype_t *datatype,
                          struct ompi_message_t **message,
                          ompi_status_public_t* status)
{
    return OMPI_SUCCESS;
}
