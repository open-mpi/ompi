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

int32_t ompi_ddt_destroy( ompi_datatype_t** dt )
{
    ompi_datatype_t* pData = *dt;

    if( pData->flags & DT_FLAG_PREDEFINED )
        return OMPI_ERROR;

    OBJ_RELEASE( pData );
    *dt = NULL;
    return OMPI_SUCCESS;
}
