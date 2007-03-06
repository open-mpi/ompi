/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

int32_t ompi_ddt_destroy( ompi_datatype_t** dt )
{
    ompi_datatype_t* pData = *dt;

    if( (pData->flags & DT_FLAG_PREDEFINED) && (pData->super.obj_reference_count <= 1) )
        return OMPI_ERROR;

    OBJ_RELEASE( pData );
    *dt = NULL;
    return OMPI_SUCCESS;
}
