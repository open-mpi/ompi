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

#include "orte_config.h"

#include "include/orte_constants.h"
#include "class/orte_value_array.h"


static void orte_value_array_construct(orte_value_array_t* array)
{
    array->array_items = NULL;
    array->array_size = 0;
    array->array_item_sizeof = 0;
    array->array_alloc_size = 0;
}

static void orte_value_array_destruct(orte_value_array_t* array)
{
    if (NULL != array->array_items)
        free(array->array_items);
}

ompi_class_t orte_value_array_t_class = {
    "orte_value_array_t",
     OBJ_CLASS(ompi_object_t),
     (ompi_construct_t)orte_value_array_construct,
     (ompi_destruct_t)orte_value_array_destruct
};


int orte_value_array_set_size(orte_value_array_t* array, size_t size)
{
#if OMPI_ENABLE_DEBUG
    if(array->array_item_sizeof == 0) {
        ompi_output(0, "orte_value_array_set_size: item size must be initialized");
        return ORTE_ERR_BAD_PARAM;
    }
#endif

    if(size > array->array_alloc_size) {
        while(array->array_alloc_size < size)
            array->array_alloc_size <<= 1;
        array->array_items = (unsigned char *)realloc(array->array_items,
            array->array_alloc_size * array->array_item_sizeof);
        if (NULL == array->array_items)
            return ORTE_ERR_OUT_OF_RESOURCE;
    }
    array->array_size = size;
    return ORTE_SUCCESS;
}

