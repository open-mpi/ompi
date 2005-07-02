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
#include "datatype/datatype_internal.h"

static inline
const ompi_datatype_t* ompi_ddt_match_size_internal( int size, uint16_t datakind, uint16_t datalang )
{
    uint32_t i;
    const ompi_datatype_t* datatype;

    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        datatype = ompi_ddt_basicDatatypes[i];
        if( (datatype->flags & DT_FLAG_DATA_LANGUAGE) != datalang )
            continue;
        if( (datatype->flags & DT_FLAG_DATA_TYPE) != datakind )
            continue;
        if( (unsigned long)size == datatype->size ) {
            return datatype;
        }
    }
    return &ompi_mpi_datatype_null;
}

const ompi_datatype_t* ompi_ddt_match_size( int size, uint16_t datakind, uint16_t datalang )
{
    if( datalang == DT_FLAG_DATA_CPP ) {
        if( datakind == DT_FLAG_DATA_COMPLEX ) 
            return ompi_ddt_match_size_internal( size, datakind, datalang );
        datalang = DT_FLAG_DATA_C;
    }
    return ompi_ddt_match_size_internal( size, datakind, datalang );
}
