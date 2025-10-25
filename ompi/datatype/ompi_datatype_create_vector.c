/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>

#include "ompi/datatype/ompi_datatype.h"

int32_t ompi_datatype_create_vector( size_t count, size_t bLength, ptrdiff_t stride,
                                     const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t *pTempData, *pData;
    ptrdiff_t extent = oldType->super.ub - oldType->super.lb;

    if( (0 == count) || (0 == bLength) ) {
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }

    pData = ompi_datatype_create( oldType->super.desc.used + 2 );
    if( (bLength == (size_t)stride) || (1 >= count) ) {  /* the elements are contiguous */
        ompi_datatype_add( pData, oldType, (size_t)count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            ompi_datatype_add( pData, oldType, count, 0, extent * stride );
        } else {
            ompi_datatype_add( pData, oldType, bLength, 0, extent );
            pTempData = pData;
            pData = ompi_datatype_create( oldType->super.desc.used + 2 + 2 );
            ompi_datatype_add( pData, pTempData, count, 0, extent * stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_create_hvector( size_t count, size_t bLength, ptrdiff_t stride,
                                      const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t *pTempData, *pData;
    ptrdiff_t extent = oldType->super.ub - oldType->super.lb;

    if( (0 == count) || (0 == bLength) ) {
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }

    pTempData = ompi_datatype_create( oldType->super.desc.used + 2 );
    if( ((extent * bLength) == (size_t)stride) || (1 >= count) ) {  /* contiguous */
        pData = pTempData;
        ompi_datatype_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            pData = pTempData;
            ompi_datatype_add( pData, oldType, count, 0, stride );
        } else {
            ompi_datatype_add( pTempData, oldType, bLength, 0, extent );
            pData = ompi_datatype_create( oldType->super.desc.used + 2 + 2 );
            ompi_datatype_add( pData, pTempData, count, 0, stride );
            OBJ_RELEASE( pTempData );
        }
    }
     *newType = pData;
    return OMPI_SUCCESS;
}
