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
#include "request/request.h"
#include "pml_example.h"

int mca_pml_example_iprobe( int src, int tag,
                        struct ompi_communicator_t *comm,
                        int *matched, ompi_status_public_t * status )
{
    return OMPI_SUCCESS;
}

int mca_pml_example_probe( int src, int tag,
                       struct ompi_communicator_t *comm,
                       ompi_status_public_t * status )
{
    return OMPI_SUCCESS;
}
