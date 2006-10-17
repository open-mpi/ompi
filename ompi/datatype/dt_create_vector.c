/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"

/* Open questions ...
 *  - how to improuve the handling of these vectors (creating a temporary datatype
 *    can be ONLY a initial solution.
 *
 */

int32_t ompi_ddt_create_vector( int count, int bLength, int stride,
                                const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent = oldType->ub - oldType->lb;
    ompi_datatype_t *pTempData, *pData;

    if( 0 == count ) {
        *newType = ompi_ddt_create( 0 );
        ompi_ddt_add( *newType, &ompi_mpi_datatype_null, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    pData = ompi_ddt_create( oldType->desc.used + 2 );
    if( (bLength == stride) || (1 >= count) ) {  /* the elements are contiguous */
        ompi_ddt_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            ompi_ddt_add( pData, oldType, count, 0, extent * stride );
        } else {
            ompi_ddt_add( pData, oldType, bLength, 0, extent );
            pTempData = pData;
            pData = ompi_ddt_create( oldType->desc.used + 2 + 2 );
            ompi_ddt_add( pData, pTempData, count, 0, extent * stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_create_hvector( int count, int bLength, MPI_Aint stride,
                                 const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent = oldType->ub - oldType->lb;
    ompi_datatype_t *pTempData, *pData;

    if( 0 == count ) {
        *newType = ompi_ddt_create( 0 );
        ompi_ddt_add( *newType, &ompi_mpi_datatype_null, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    pTempData = ompi_ddt_create( oldType->desc.used + 2 );
    if( ((extent * bLength) == stride) || (1 >= count) ) {  /* contiguous */
        pData = pTempData;
        ompi_ddt_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            pData = pTempData;
            ompi_ddt_add( pData, oldType, count, 0, stride );
        } else {
            ompi_ddt_add( pTempData, oldType, bLength, 0, extent );
            pData = ompi_ddt_create( oldType->desc.used + 2 + 2 );
            ompi_ddt_add( pData, pTempData, count, 0, stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OMPI_SUCCESS;
}
