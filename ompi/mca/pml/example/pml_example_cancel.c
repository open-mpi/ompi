/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_example.h"

int mca_pml_example_cancel(ompi_request_t* request)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_cancelled(ompi_request_t* request, int* flag)
{
    if(NULL != flag)
        *flag = 0;
    return OMPI_SUCCESS;
}

