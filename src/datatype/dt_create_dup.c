/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "datatype/datatype.h"
#include "datatype/datatype_internal.h"

int ompi_ddt_duplicate( const dt_desc_t* oldType, dt_desc_t** newType )
{
    dt_desc_t* pdt = ompi_ddt_create( oldType->desc.used );
    void* temp = pdt->desc.desc; /* temporary copy of the desc pointer */

    memcpy( pdt, oldType, sizeof(dt_desc_t) );
    pdt->desc.desc = temp;
    memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * oldType->desc.used );
    pdt->id  = 0;
    pdt->args = NULL;
    /* TODO: if the data was commited update the opt_desc field */
    *newType = pdt;
    return OMPI_SUCCESS;
}

int ompi_ddt_create_contiguous( int count, const dt_desc_t* oldType, 
				dt_desc_t** newType )
{
   dt_desc_t* pdt = ompi_ddt_create( oldType->desc.used + 2 );
   ompi_ddt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
   *newType = pdt;
   return OMPI_SUCCESS;
}
