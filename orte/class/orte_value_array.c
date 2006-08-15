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

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/class/orte_value_array.h"


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

opal_class_t orte_value_array_t_class = {
    "orte_value_array_t",
     OBJ_CLASS(opal_object_t),
     (opal_construct_t)orte_value_array_construct,
     (opal_destruct_t)orte_value_array_destruct
};


int orte_value_array_set_size(orte_value_array_t* array, orte_std_cntr_t size)
{
#if OMPI_ENABLE_DEBUG
    if(array->array_item_sizeof == 0) {
        opal_output(0, "orte_value_array_set_size: item size must be initialized");
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

