/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include <string.h>

#include "mca/bml/bml.h"
#include "bml_base_btl.h"


static void mca_bml_base_btl_array_construct(mca_bml_base_btl_array_t* array)
{
    array->bml_btls = NULL;
    array->arr_size = 0;
    array->arr_index = 0;
    array->arr_reserve = 0;
}


static void mca_bml_base_btl_array_destruct(mca_bml_base_btl_array_t* array)
{
    if(NULL != array->bml_btls)
        free(array->bml_btls);
}

OBJ_CLASS_INSTANCE(
    mca_bml_base_btl_array_t,
    opal_object_t,
    mca_bml_base_btl_array_construct,
    mca_bml_base_btl_array_destruct
);

int mca_bml_base_btl_array_reserve(mca_bml_base_btl_array_t* array, size_t size)
{
    size_t old_len = sizeof(mca_bml_base_btl_t)*array->arr_reserve;
    size_t new_len = sizeof(mca_bml_base_btl_t)*size;
    if(old_len >= new_len)
        return OMPI_SUCCESS;
    
    array->bml_btls = realloc(array->bml_btls, new_len);
    if(NULL == array->bml_btls)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memset((unsigned char*)array->bml_btls + old_len, 0, new_len-old_len);
    array->arr_reserve = size;
    return OMPI_SUCCESS;
}


