/*
 * $HEADER$
 */

#include "lam/lfc/lam_value_array.h"


static void lam_value_array_construct(lam_value_array_t* array)
{
    array->array_items = NULL;
    array->array_size = 0;
    array->array_item_sizeof = 0;
    array->array_alloc_size = 0;
}

static void lam_value_array_destruct(lam_value_array_t* array)
{
    if (NULL != array->array_items)
        free(array->array_items);
}

lam_class_t lam_value_array_t_class = {
    "lam_value_array_t",
     OBJ_CLASS(lam_object_t),
     (lam_construct_t)lam_value_array_construct,
     (lam_destruct_t)lam_value_array_destruct
};


int lam_value_array_set_size(lam_value_array_t* array, size_t size)
{
#if LAM_ENABLE_DEBUG
    if(array->array_item_sizeof == 0) {
        lam_output(0, "lam_value_array_set_size: item size must be initialized");
        return LAM_ERR_BAD_PARAM;
    }
#endif

    if(size > array->array_alloc_size) {
        while(array->array_alloc_size < size)
            array->array_alloc_size <<= 1;
        array->array_items = realloc(array->array_items,
            array->array_alloc_size * array->array_item_sizeof);
        if (NULL == array->array_items)
            return LAM_ERR_OUT_OF_RESOURCE;
    }
    array->array_size = size;
    return LAM_SUCCESS;
}

