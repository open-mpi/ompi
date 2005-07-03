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

#include <string.h>

#include "mca/pml/pml.h"
#include "pml_ob1_endpoint.h"


static void mca_pml_ob1_ep_array_construct(mca_pml_ob1_ep_array_t* array)
{
    array->arr_endpoints = NULL;
    array->arr_size = 0;
    array->arr_index = 0;
    array->arr_reserve = 0;
}


static void mca_pml_ob1_ep_array_destruct(mca_pml_ob1_ep_array_t* array)
{
    if(NULL != array->arr_endpoints)
        free(array->arr_endpoints);
}

OBJ_CLASS_INSTANCE(
    mca_pml_ob1_ep_array_t,
    opal_object_t,
    mca_pml_ob1_ep_array_construct,
    mca_pml_ob1_ep_array_destruct
);

int mca_pml_ob1_ep_array_reserve(mca_pml_ob1_ep_array_t* array, size_t size)
{
    size_t old_len = sizeof(mca_pml_ob1_endpoint_t)*array->arr_reserve;
    size_t new_len = sizeof(mca_pml_ob1_endpoint_t)*size;
    if(old_len >= new_len)
        return OMPI_SUCCESS;
    
    array->arr_endpoints = realloc(array->arr_endpoints, new_len);
    if(NULL == array->arr_endpoints)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memset((unsigned char*)array->arr_endpoints + old_len, 0, new_len-old_len);
    array->arr_reserve = size;
    return OMPI_SUCCESS;
}

