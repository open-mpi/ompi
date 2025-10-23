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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>

#include "ompi/datatype/ompi_datatype.h"


/* We try to merge together data that are contiguous */
int32_t ompi_datatype_create_indexed( size_t count, const ompi_count_array_t pBlockLength, const ompi_disp_array_t pDisp,
                                      const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent, disp, endat;
    ompi_datatype_t* pdt;
    size_t dLength;
    size_t i;

    /* ignore all cases that lead to an empty type */
    ompi_datatype_type_size(oldType, &dLength);
    for( i = 0; (i < count) && (0 == ompi_count_array_get(pBlockLength, i)); i++ );  /* find first non zero */
    if( (i == count) || (0 == dLength) ) {
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }

    disp = ompi_disp_array_get(pDisp, i);
    dLength = ompi_count_array_get(pBlockLength, i);
    endat = disp + dLength;
    ompi_datatype_type_extent( oldType, &extent );

    pdt = ompi_datatype_create( (count - i) * (2 + oldType->super.desc.used) );
    for( i += 1; i < count; i++ ) {
        if( 0 == ompi_count_array_get(pBlockLength, i) )  /* ignore empty length */
            continue;
        if( endat == ompi_disp_array_get(pDisp, i) ) { /* contiguous with the previsious */
            dLength += ompi_count_array_get(pBlockLength, i);
            endat += ompi_count_array_get(pBlockLength, i);
        } else {
            ompi_datatype_add( pdt, oldType, dLength, disp * extent, extent );
            disp = ompi_disp_array_get(pDisp, i);
            dLength = ompi_count_array_get(pBlockLength, i);
            endat = disp + dLength;
        }
    }
    ompi_datatype_add( pdt, oldType, dLength, disp * extent, extent );

    *newType = pdt;
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_create_hindexed( size_t count, const ompi_count_array_t pBlockLength, const ompi_disp_array_t pDisp,
                                       const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent, disp, endat;
    ompi_datatype_t* pdt;
    size_t dLength;
    size_t i;

    /* ignore all cases that lead to an empty type */
    ompi_datatype_type_size(oldType, &dLength);
    for( i = 0; (i < count) && (0 == ompi_count_array_get(pBlockLength, i)); i++ );  /* find first non zero */
    if( (i == count) || (0 == dLength) ) {
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }

    ompi_datatype_type_extent( oldType, &extent );
    disp = ompi_disp_array_get(pDisp, i);
    dLength = ompi_count_array_get(pBlockLength, i);
    endat = disp + dLength * extent;

    pdt = ompi_datatype_create( (count - i) * (2 + oldType->super.desc.used) );
    for( i += 1; i < count; i++ ) {
        if( 0 == ompi_count_array_get(pBlockLength, i) )  /* ignore empty length */
            continue;
        if( endat == ompi_disp_array_get(pDisp, i) ) { /* contiguous with the previsious */
            dLength += ompi_count_array_get(pBlockLength, i);
            endat += ompi_count_array_get(pBlockLength, i) * extent;
        } else {
            ompi_datatype_add( pdt, oldType, dLength, disp, extent );
            disp = ompi_disp_array_get(pDisp, i);
            dLength = ompi_count_array_get(pBlockLength, i);
            endat = disp + dLength * extent;
        }
    }
    ompi_datatype_add( pdt, oldType, dLength, disp, extent );

    *newType = pdt;
    return OMPI_SUCCESS;
}


int32_t ompi_datatype_create_indexed_block( size_t count, size_t bLength, const ompi_disp_array_t pDisp,
                                            const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent, disp, endat;
    ompi_datatype_t* pdt;
    size_t dLength;
    size_t i;

    if( (count == 0) || (bLength == 0) ) {
        return ompi_datatype_duplicate(&ompi_mpi_datatype_null.dt, newType);
    }
    ompi_datatype_type_extent( oldType, &extent );
    pdt = ompi_datatype_create( count * (2 + oldType->super.desc.used) );
    disp = ompi_disp_array_get(pDisp, 0);
    dLength = bLength;
    endat = disp + dLength;
    for( i = 1; i < count; i++ ) {
        if( endat == ompi_disp_array_get(pDisp, i) ) {
            /* contiguous with the previsious */
            dLength += bLength;
            endat += bLength;
        } else {
            ompi_datatype_add( pdt, oldType, dLength, disp * extent, extent );
            disp = ompi_disp_array_get(pDisp, i);
            dLength = bLength;
            endat = disp + bLength;
        }
    }
    ompi_datatype_add( pdt, oldType, dLength, disp * extent, extent );

    *newType = pdt;
    return OMPI_SUCCESS;
}

int32_t ompi_datatype_create_hindexed_block( size_t count, size_t bLength, const ompi_disp_array_t pDisp,
                                             const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ptrdiff_t extent, disp, endat;
    ompi_datatype_t* pdt;
    size_t dLength;
    size_t i;

    if( (count == 0) || (bLength == 0) ) {
        return ompi_datatype_duplicate(&ompi_mpi_datatype_null.dt, newType);
    }
    ompi_datatype_type_extent( oldType, &extent );
    pdt = ompi_datatype_create( count * (2 + oldType->super.desc.used) );
    disp = ompi_disp_array_get(pDisp, 0);
    dLength = bLength;
    endat = disp + dLength * extent;
    for( i = 1; i < count; i++ ) {
        if( endat == ompi_disp_array_get(pDisp, i) ) {
            /* contiguous with the previsious */
            dLength += bLength;
            endat += bLength * extent;
        } else {
            ompi_datatype_add( pdt, oldType, dLength, disp, extent );
            disp = ompi_disp_array_get(pDisp, i);
            dLength = bLength;
            endat = disp + bLength * extent;
        }
    }
    ompi_datatype_add( pdt, oldType, dLength, disp, extent );

    *newType = pdt;
    return OMPI_SUCCESS;
}
