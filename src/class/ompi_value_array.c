/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_value_array.h"


static void ompi_value_array_construct(ompi_value_array_t* array)
{
    array->array_items = NULL;
    array->array_size = 0;
    array->array_item_sizeof = 0;
    array->array_alloc_size = 0;
}

static void ompi_value_array_destruct(ompi_value_array_t* array)
{
    if (NULL != array->array_items)
        free(array->array_items);
}

ompi_class_t ompi_value_array_t_class = {
    "ompi_value_array_t",
     OBJ_CLASS(ompi_object_t),
     (ompi_construct_t)ompi_value_array_construct,
     (ompi_destruct_t)ompi_value_array_destruct
};


int ompi_value_array_set_size(ompi_value_array_t* array, size_t size)
{
#if OMPI_ENABLE_DEBUG
    if(array->array_item_sizeof == 0) {
        ompi_output(0, "ompi_value_array_set_size: item size must be initialized");
        return OMPI_ERR_BAD_PARAM;
    }
#endif

    if(size > array->array_alloc_size) {
        while(array->array_alloc_size < size)
            array->array_alloc_size <<= 1;
        array->array_items = (unsigned char *)realloc(array->array_items,
            array->array_alloc_size * array->array_item_sizeof);
        if (NULL == array->array_items)
            return OMPI_ERR_OUT_OF_RESOURCE;
    }
    array->array_size = size;
    return OMPI_SUCCESS;
}

