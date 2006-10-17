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
#include "ompi/datatype/datatype_internal.h"

extern int32_t ompi_ddt_number_of_predefined_data;

const ompi_datatype_t*
ompi_ddt_match_size( int size, uint16_t datakind, uint16_t datalang )
{
    int32_t i;
    const ompi_datatype_t* datatype;

    /* If we're not looking for a complex C++ type then set the default type to C */
    if( datalang == DT_FLAG_DATA_CPP ) {
        if( datakind != DT_FLAG_DATA_COMPLEX )
            datalang = DT_FLAG_DATA_C;
    }

    for( i = 0; i < ompi_ddt_number_of_predefined_data; i++ ) {

        datatype = (ompi_datatype_t*)ompi_pointer_array_get_item(ompi_datatype_f_to_c_table, i);

        if( (datatype->flags & DT_FLAG_DATA_LANGUAGE) != datalang )
            continue;
        if( (datatype->flags & DT_FLAG_DATA_TYPE) != datakind )
            continue;
        if( (size_t)size == datatype->size ) {
            return datatype;
        }
    }
    return &ompi_mpi_datatype_null;
}
