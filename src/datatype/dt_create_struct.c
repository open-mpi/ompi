/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "datatype.h"

int ompi_ddt_create_struct( int count, const int* pBlockLength, const long* pDisp,
                            const dt_desc_t ** pTypes, dt_desc_t** newType )
{
    int i;
    long disp = 0, endto, lastExtent, lastDisp;
    int lastBlock;
    dt_desc_t *pdt, *lastType;

    /* if we compute the total number of elements before we can
     * avoid increasing the size of the desc array often.
     */
    for( i = 0; i < count; i++ ) {
        disp += pTypes[i]->desc.used;
        if( pBlockLength[i] != 1 ) disp += 2;
    }
    lastType = (dt_desc_t*)pTypes[0];
    lastBlock = pBlockLength[0];
    lastExtent = lastType->ub - lastType->lb;
    lastDisp = pDisp[0];
    endto = pDisp[0] + lastExtent * lastBlock;

    for( i = 1; i < count; i++ ) {
        if( (pTypes[i] == lastType) && (pDisp[i] == endto) ) {
            lastBlock += pBlockLength[i];
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            disp += lastType->desc.used;
            if( lastBlock > 1 ) disp += 2;
            lastType = (dt_desc_t*)pTypes[i];
            lastExtent = lastType->ub - lastType->lb;
            lastBlock = pBlockLength[i];
            lastDisp = pDisp[i];
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    disp += lastType->desc.used;
    if( lastBlock != 1 ) disp += 2;

    lastType = (dt_desc_t*)pTypes[0];
    lastBlock = pBlockLength[0];
    lastExtent = lastType->ub - lastType->lb;
    lastDisp = pDisp[0];
    endto = pDisp[0] + lastExtent * lastBlock;

    pdt = ompi_ddt_create( disp );

    /* Do again the same loop but now add the elements */
    for( i = 1; i < count; i++ ) {
        if( (pTypes[i] == lastType) && (pDisp[i] == endto) ) {
            lastBlock += pBlockLength[i];
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            ompi_ddt_add( pdt, lastType, lastBlock, lastDisp, lastExtent );
            lastType = (dt_desc_t*)pTypes[i];
            lastExtent = lastType->ub - lastType->lb;
            lastBlock = pBlockLength[i];
            lastDisp = pDisp[i];
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    ompi_ddt_add( pdt, lastType, lastBlock, lastDisp, lastExtent );

    *newType = pdt;
    return OMPI_SUCCESS;
}
