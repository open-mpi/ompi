/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>

#include "ompi/datatype/ompi_datatype.h"

/* Open questions ...
 *  - how to improuve the handling of these vectors (creating a temporary datatype
 *    can be ONLY a initial solution.
 *
 */

int32_t ompi_datatype_create_vector( int count, int bLength, int stride,
                                     const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t *pTempData, *pData;
    OPAL_PTRDIFF_TYPE extent = oldType->super.ub - oldType->super.lb;


    if( 0 == count ) {
        *newType = ompi_datatype_create( 0 );
        ompi_datatype_add( *newType, &ompi_mpi_datatype_null.dt, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    if( (bLength == stride) || (1 >= count) ) {  /* the elements are contiguous */
        pData = ompi_datatype_create( oldType->super.desc.used + 2 );
        ompi_datatype_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            pData = ompi_datatype_create( oldType->super.desc.used + 2 );
            ompi_datatype_add( pData, oldType, count, 0, extent * stride );
        } else {
            pTempData = ompi_datatype_create_temporary( oldType->super.desc.used + 2 ); 
            ompi_datatype_add( pTempData , oldType, bLength, 0, extent );
            pData = ompi_datatype_create( oldType->super.desc.used + 2 + 2 );
            ompi_datatype_add( pData, pTempData, count, 0, extent * stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_create_hvector( int count, int bLength, OPAL_PTRDIFF_TYPE stride,
                                      const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t *pTempData, *pData;
    OPAL_PTRDIFF_TYPE extent = oldType->super.ub - oldType->super.lb;

    if( 0 == count ) {
        *newType = ompi_datatype_create( 0 );
        ompi_datatype_add( *newType, &ompi_mpi_datatype_null.dt, 0, 0, 0);
        return OMPI_SUCCESS;
    }

    if( ((extent * bLength) == stride) || (1 >= count) ) {  /* contiguous */
        pData = ompi_datatype_create( oldType->super.desc.used + 2 );
        ompi_datatype_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            pData = ompi_datatype_create( oldType->super.desc.used + 2 );
            ompi_datatype_add( pData, oldType, count, 0, stride );
        } else {
            pTempData =  ompi_datatype_create_temporary( oldType->super.desc.used + 2 );
            ompi_datatype_add( pTempData, oldType, bLength, 0, extent );
            pData = ompi_datatype_create( oldType->super.desc.used + 2 + 2 );
            ompi_datatype_add( pData, pTempData, count, 0, stride );
            OBJ_RELEASE( pTempData );
        }
    }
     *newType = pData;
    return OMPI_SUCCESS;
}
