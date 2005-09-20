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

#include "pml_portals.h"

int mca_pml_portals_cancel(ompi_request_t* request)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_cancelled(ompi_request_t* request, int* flag)
{
    if(NULL != flag)
        *flag = 0;
    return OMPI_SUCCESS;
}

