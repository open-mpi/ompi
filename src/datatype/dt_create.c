/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"
#include "limits.h"

static void __get_free_dt_struct(  dt_desc_t* pData )
{
    int i;

    pData->size            = 0;
    pData->id              = 0;
    pData->nbElems         = 0;
    pData->bdt_used        = 0;
    for( i = 0; i < DT_MAX_PREDEFINED; i++ )
        pData->btypes[i]    = 0;
    pData->btypes[DT_LOOP] = 1;

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
}

static void __destroy_ddt_struct( dt_desc_t* pData )
{
    if( pData->desc.desc != NULL ) free( pData->desc.desc );
    pData->desc.desc   = NULL;
    pData->desc.length = 0;
    pData->desc.used   = 0;
    if( pData->opt_desc.desc != NULL ) free( pData->opt_desc.desc );
    pData->opt_desc.desc   = NULL;
    pData->opt_desc.length = 0;
    pData->opt_desc.used   = 0;
    if( pData->args != NULL ) free( pData->args );
    pData->args = NULL;
    if( NULL != ompi_pointer_array_get_item(ompi_datatype_f_to_c_table, pData->d_f_to_c_index) ){
        ompi_pointer_array_set_item( ompi_datatype_f_to_c_table, pData->d_f_to_c_index, NULL );
    }

}

OBJ_CLASS_INSTANCE(ompi_datatype_t, ompi_object_t, __get_free_dt_struct, __destroy_ddt_struct );

dt_desc_t* ompi_ddt_create( int expectedSize )
{
    dt_desc_t* pdt = (dt_desc_t*)OBJ_NEW(ompi_datatype_t);

    if( expectedSize == -1 ) expectedSize = DT_INCREASE_STACK;
    pdt->desc.length = expectedSize + 1;  /* one for the fake elem at the end */
    pdt->desc.used   = 0;
    pdt->desc.desc   = (dt_elem_desc_t*)calloc(pdt->desc.length, sizeof(dt_elem_desc_t));
    memset( pdt->name, 0, MPI_MAX_OBJECT_NAME );
    return pdt;
}

int ompi_ddt_create_resized( dt_desc_t* oldType, long lb, long extent, dt_desc_t** newType )
{
    ompi_ddt_duplicate( oldType, newType );
    (*newType)->lb = lb;
    (*newType)->ub = lb + extent;
    return OMPI_SUCCESS;
}
