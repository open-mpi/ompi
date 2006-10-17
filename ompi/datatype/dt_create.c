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
#include "limits.h"
#include "ompi/attribute/attribute.h"

static void __get_free_dt_struct( ompi_datatype_t* pData )
{
    int i;

    pData->size               = 0;
    pData->id                 = 0;
    pData->nbElems            = 0;
    pData->bdt_used           = 0;
    for( i = 0; i < DT_MAX_PREDEFINED; i++ )
        pData->btypes[i]      = 0;
    pData->btypes[DT_LOOP]    = 0;

    pData->opt_desc.desc      = NULL;
    pData->opt_desc.length    = 0;
    pData->opt_desc.used      = 0;
    pData->args               = NULL;
    pData->align              = 1;
    pData->flags              = DT_FLAG_CONTIGUOUS;
    pData->true_lb            = LONG_MAX;
    pData->true_ub            = LONG_MIN;
    pData->lb                 = LONG_MAX;
    pData->ub                 = LONG_MIN;
    pData->d_f_to_c_index     = ompi_pointer_array_add(ompi_datatype_f_to_c_table, pData);
    pData->d_keyhash          = NULL;
    pData->name[0]            = '\0';
    pData->packed_description = NULL;
}

static void __destroy_ddt_struct( ompi_datatype_t* datatype )
{
    if( datatype->desc.desc != NULL ) {
        free( datatype->desc.desc );
        datatype->desc.length = 0;
        datatype->desc.used   = 0;
    }
    if( datatype->opt_desc.desc != NULL ) {
        if( datatype->opt_desc.desc != datatype->desc.desc )
            free( datatype->opt_desc.desc );
        datatype->opt_desc.length = 0;
        datatype->opt_desc.used   = 0;
        datatype->opt_desc.desc   = NULL;
    }
    /**
     * As the default description and the optimized description can point to the
     * same memory location we should keep the default location pointer until we
     * know what we should do with the optimized description.
     */
    datatype->desc.desc   = NULL;

    if( NULL != datatype->args ) {
        ompi_ddt_release_args( datatype );
        datatype->args = NULL;
    }
    if( NULL != datatype->packed_description ) {
        free( datatype->packed_description );
        datatype->packed_description = NULL;
    }
    if( NULL != ompi_pointer_array_get_item(ompi_datatype_f_to_c_table, datatype->d_f_to_c_index) ){
        ompi_pointer_array_set_item( ompi_datatype_f_to_c_table, datatype->d_f_to_c_index, NULL );
    }
    /* any pending attributes ? */
    if (NULL != datatype->d_keyhash) {
        ompi_attr_delete_all( TYPE_ATTR, datatype, datatype->d_keyhash );
        OBJ_RELEASE( datatype->d_keyhash );
    }
    /* make sure the name is set to empty */
    datatype->name[0] = '\0';
}

OBJ_CLASS_INSTANCE(ompi_datatype_t, opal_object_t, __get_free_dt_struct, __destroy_ddt_struct );

ompi_datatype_t* ompi_ddt_create( int32_t expectedSize )
{
    ompi_datatype_t* datatype = (ompi_datatype_t*)OBJ_NEW(ompi_datatype_t);

    if( expectedSize == -1 ) expectedSize = DT_INCREASE_STACK;
    datatype->desc.length = expectedSize + 1;  /* one for the fake elem at the end */
    datatype->desc.used   = 0;
    datatype->desc.desc   = (dt_elem_desc_t*)calloc(datatype->desc.length, sizeof(dt_elem_desc_t));
    memset( datatype->name, 0, MPI_MAX_OBJECT_NAME );
    return datatype;
}

int32_t ompi_ddt_create_resized( const ompi_datatype_t* oldType, MPI_Aint lb, MPI_Aint extent, ompi_datatype_t** newType )
{
    ompi_ddt_duplicate( oldType, newType );
    (*newType)->lb = lb;
    (*newType)->ub = lb + extent;

    (*newType)->flags &= ~DT_FLAG_NO_GAPS;
    if( (extent == (ptrdiff_t)oldType->size) && (oldType->flags & DT_FLAG_CONTIGUOUS) ) {
        (*newType)->flags |= DT_FLAG_NO_GAPS;
    }
    return OMPI_SUCCESS;
}
