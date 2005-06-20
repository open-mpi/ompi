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

int32_t ompi_ddt_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t* pdt = ompi_ddt_create( oldType->desc.used );
    void* temp = pdt->desc.desc; /* temporary copy of the desc pointer */
    int32_t old_index = pdt->d_f_to_c_index;

    memcpy( pdt, oldType, sizeof(ompi_datatype_t) );
    pdt->desc.desc = temp;
    pdt->flags &= (~DT_FLAG_PREDEFINED);
    /* ompi_ddt_create() creates a new f_to_c index that was saved
       before we did the memcpy, above */
    pdt->d_f_to_c_index = old_index;
    /* Set the keyhash to NULL -- copying attributes is *only* done at
       the top level (specifically, MPI_TYPE_DUP). */
    pdt->d_keyhash = NULL;

    memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * oldType->desc.used );
    pdt->id  = 0;
    pdt->args = NULL;
    /* TODO: if the data was commited update the opt_desc field */
    if( 0 != oldType->opt_desc.used ) {
        pdt->opt_desc.desc = malloc( oldType->opt_desc.used * sizeof(dt_elem_desc_t) );
        pdt->opt_desc.length = oldType->opt_desc.used;
        pdt->opt_desc.used = oldType->opt_desc.used;
        memcpy( pdt->opt_desc.desc, oldType->opt_desc.desc, oldType->opt_desc.used * sizeof(dt_elem_desc_t) );
    }
    *newType = pdt;
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_create_contiguous( int count, const ompi_datatype_t* oldType, 
				    ompi_datatype_t** newType )
{
   ompi_datatype_t* pdt = ompi_ddt_create( oldType->desc.used + 2 );
   ompi_ddt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
   *newType = pdt;
   return OMPI_SUCCESS;
}
