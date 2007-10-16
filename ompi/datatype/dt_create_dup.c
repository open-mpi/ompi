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

/*
 * As the new type has the same commit state as the old one, I have to copy the fake
 * DT_END_LOOP from the description (both normal and optimized).
 */
int32_t ompi_ddt_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    int32_t desc_length = oldType->desc.used + 1;  /* +1 because of the fake DT_END_LOOP entry */
    ompi_datatype_t* pdt = ompi_ddt_create( desc_length );
    dt_elem_desc_t* temp = pdt->desc.desc; /* temporary copy of the desc pointer */
    int32_t old_index = pdt->d_f_to_c_index;

    memcpy( pdt, oldType, sizeof(ompi_datatype_t) );
    pdt->super.obj_reference_count = 1;
    pdt->desc.desc = temp;
    pdt->flags &= (~DT_FLAG_PREDEFINED);
    /* ompi_ddt_create() creates a new f_to_c index that was saved
       before we did the memcpy, above */
    pdt->d_f_to_c_index = old_index;
    /* Set the keyhash to NULL -- copying attributes is *only* done at
       the top level (specifically, MPI_TYPE_DUP). */
    pdt->d_keyhash = NULL;

    /**
     * Allow duplication of MPI_UB and MPI_LB.
     */
    if( 0 != oldType->desc.used ) {
        memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * desc_length );
        /* TODO: if the data was commited update the opt_desc field */
        if( 0 != oldType->opt_desc.used ) {
            desc_length = pdt->opt_desc.used + 1;
            pdt->opt_desc.desc = (dt_elem_desc_t*)malloc( desc_length * sizeof(dt_elem_desc_t) );
            /*
             * Yes, the pdt->opt_desc.length is just the opt_desc.used of the old Type.
             */
            pdt->opt_desc.length = oldType->opt_desc.used;
            pdt->opt_desc.used = oldType->opt_desc.used;
            memcpy( pdt->opt_desc.desc, oldType->opt_desc.desc, desc_length * sizeof(dt_elem_desc_t) );
        }
    }
    pdt->id  = oldType->id;  /* preserve the default id. This allow us to
                              * copy predefined types. */
    pdt->args = NULL;
    *newType = pdt;
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_create_contiguous( int count, const ompi_datatype_t* oldType,
				    ompi_datatype_t** newType )
{
    ompi_datatype_t* pdt;

    if( 0 == count ) {
        pdt = ompi_ddt_create( 0 );
        ompi_ddt_add( pdt, &ompi_mpi_datatype_null, 0, 0, 0 );
    } else {
        pdt = ompi_ddt_create( oldType->desc.used + 2 );
        ompi_ddt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
    }
    *newType = pdt;
    return OMPI_SUCCESS;
}
