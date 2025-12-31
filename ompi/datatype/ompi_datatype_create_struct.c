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

int32_t ompi_datatype_create_struct( size_t count, const ompi_count_array_t pBlockLength, const ompi_disp_array_t pDisp,
                                     ompi_datatype_t* const * pTypes, ompi_datatype_t** newType )
{
    ptrdiff_t disp = 0, endto, lastExtent, lastDisp;
    ompi_datatype_t *pdt, *lastType;
    size_t i, start_from;
    size_t lastBlock;

    /* Find first non-zero length element */
    for( i = 0; (i < count) && (0 == ompi_count_array_get(pBlockLength, i)); i++ );
    if( i == count ) {  /* either nothing or nothing relevant */
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }
    /* compute the total number of elements before we can
     * avoid increasing the size of the desc array often.
     */
    start_from = i;
    lastType = (ompi_datatype_t*)pTypes[start_from];
    lastBlock = ompi_count_array_get(pBlockLength, start_from);
    lastExtent = lastType->super.ub - lastType->super.lb;
    lastDisp = ompi_disp_array_get(pDisp, start_from);
    endto = lastDisp + lastExtent * lastBlock;

    for( i = (start_from + 1); i < count; i++ ) {
        if( (pTypes[i] == lastType) && (ompi_disp_array_get(pDisp, i) == endto) ) {
            lastBlock += ompi_count_array_get(pBlockLength, i);
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            disp += lastType->super.desc.used;
            if( lastBlock > 1 ) disp += 2;
            lastType = (ompi_datatype_t*)pTypes[i];
            lastExtent = lastType->super.ub - lastType->super.lb;
            lastBlock = ompi_count_array_get(pBlockLength, i);
            lastDisp = ompi_disp_array_get(pDisp, i);
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    disp += lastType->super.desc.used;
    if( lastBlock != 1 ) disp += 2;

    lastType = (ompi_datatype_t*)pTypes[start_from];
    lastBlock = ompi_count_array_get(pBlockLength, start_from);
    lastExtent = lastType->super.ub - lastType->super.lb;
    lastDisp = ompi_disp_array_get(pDisp, start_from);
    endto = lastDisp + lastExtent * lastBlock;

    pdt = ompi_datatype_create( (int32_t)disp );

    /* Do again the same loop but now add the elements */
    for( i = (start_from + 1); i < count; i++ ) {
        if( (pTypes[i] == lastType) && (ompi_disp_array_get(pDisp, i) == endto) ) {
            lastBlock += ompi_count_array_get(pBlockLength, i);
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            ompi_datatype_add( pdt, lastType, lastBlock, lastDisp, lastExtent );
            lastType = (ompi_datatype_t*)pTypes[i];
            lastExtent = lastType->super.ub - lastType->super.lb;
            lastBlock = ompi_count_array_get(pBlockLength, i);
            lastDisp = ompi_disp_array_get(pDisp, i);
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    ompi_datatype_add( pdt, lastType, lastBlock, lastDisp, lastExtent );

     *newType = pdt;
    return OMPI_SUCCESS;
}
