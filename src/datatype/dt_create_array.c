/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "datatype/datatype.h"

int32_t ompi_ddt_create_subarray( int ndims, const int* pSizes, const int* pSubSizes, const int* pStarts,
                             int order, const dt_desc_t* oldType, dt_desc_t** newType )
{
   return OMPI_ERR_NOT_IMPLEMENTED;
}

int32_t ompi_ddt_create_darray( int size, int rank, int ndims, const int* pGSizes, const int *pDistrib,
                            const int* pDArgs, const int* pPSizes, int order, const dt_desc_t* oldType,
                            dt_desc_t** newType )
{
   return OMPI_ERR_NOT_IMPLEMENTED;
}

