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
#include "limits.h"
#include "attribute/attribute.h"

static void __get_free_dt_struct( ompi_datatype_t* pData )
{
    int i;

    pData->size            = 0;
    pData->id              = 0;
    pData->nbElems         = 0;
    pData->bdt_used        = 0;
    for( i = 0; i < DT_MAX_PREDEFINED; i++ )
        pData->btypes[i]    = 0;
    pData->btypes[DT_LOOP] = 0;

    pData->opt_desc.desc   = NULL;
    pData->opt_desc.length = 0;
    pData->opt_desc.used   = 0;
    pData->args            = NULL;
    pData->align           = 1;
    pData->flags           = DT_FLAG_CONTIGUOUS;
    pData->true_lb         = LONG_MAX;
    pData->true_ub         = LONG_MIN;
    pData->lb              = LONG_MAX;
    pData->ub              = LONG_MIN;
    pData->d_f_to_c_index  = ompi_pointer_array_add(ompi_datatype_f_to_c_table, pData);
    pData->d_keyhash       = NULL;
}

static void __destroy_ddt_struct( ompi_datatype_t* pData )
{
    if( pData->desc.desc != NULL ) free( pData->desc.desc );
    pData->desc.desc   = NULL;
    pData->desc.length = 0;
    pData->desc.used   = 0;
    if( pData->opt_desc.desc != NULL ) free( pData->opt_desc.desc );
    pData->opt_desc.desc   = NULL;
    pData->opt_desc.length = 0;
    pData->opt_desc.used   = 0;
    if( pData->args != NULL ) ompi_ddt_release_args( pData );
    pData->args = NULL;
    if( NULL != ompi_pointer_array_get_item(ompi_datatype_f_to_c_table, pData->d_f_to_c_index) ){
        ompi_pointer_array_set_item( ompi_datatype_f_to_c_table, pData->d_f_to_c_index, NULL );
    }
    /* any pending attributes ? */
    if (NULL != pData->d_keyhash) {
        ompi_attr_delete_all( TYPE_ATTR, pData, pData->d_keyhash );
        OBJ_RELEASE( pData->d_keyhash );
    }
}

OBJ_CLASS_INSTANCE(ompi_datatype_t, opal_object_t, __get_free_dt_struct, __destroy_ddt_struct );

ompi_datatype_t* ompi_ddt_create( int32_t expectedSize )
{
    ompi_datatype_t* pdt = (ompi_datatype_t*)OBJ_NEW(ompi_datatype_t);

    if( expectedSize == -1 ) expectedSize = DT_INCREASE_STACK;
    pdt->desc.length = expectedSize + 1;  /* one for the fake elem at the end */
    pdt->desc.used   = 0;
    pdt->desc.desc   = (dt_elem_desc_t*)calloc(pdt->desc.length, sizeof(dt_elem_desc_t));
    memset( pdt->name, 0, MPI_MAX_OBJECT_NAME );
    return pdt;
}

int32_t ompi_ddt_create_resized( const ompi_datatype_t* oldType, long lb, long extent, ompi_datatype_t** newType )
{
    ompi_ddt_duplicate( oldType, newType );
    (*newType)->lb = lb;
    (*newType)->ub = lb + extent;
    return OMPI_SUCCESS;
}
