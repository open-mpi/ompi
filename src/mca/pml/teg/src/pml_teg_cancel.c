/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"


int mca_pml_teg_cancel(ompi_request_t* request)
{
    return OMPI_SUCCESS;
}

int mca_pml_teg_cancelled(ompi_request_t* request, int* flag)
{
    if(NULL != flag)
        *flag = 0;
    return OMPI_SUCCESS;
}

