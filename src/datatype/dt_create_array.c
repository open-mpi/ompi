/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"

int ompi_ddt_create_subarray( int ndims, int* pSizes, int* pSubSizes, int* pStarts,
                             int order, dt_desc_t* oldType, dt_desc_t** newType )
{
   return OMPI_ERR_NOT_IMPLEMENTED;
}

int ompi_ddt_create_darray( int size, int rank, int ndims, int* pGSizes, int *pDistrib,
                           int* pDArgs, int* pPSizes, int order, dt_desc_t* oldType,
                           dt_desc_t** newType )
{
   return OMPI_ERR_NOT_IMPLEMENTED;
}

