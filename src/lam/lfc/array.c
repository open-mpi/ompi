/*
 * $HEADER$
 */

#include <string.h>

#include "lam/lfc/array.h"

#define ARR_BLK_SZ      20


lam_class_info_t lam_array_cls = {"lam_array_t", &lam_object_cls, 
    (class_init_t) lam_arr_init, (class_destroy_t) lam_arr_destroy};

/*
 *
 *      lam_list_t interface
 *
 */

void lam_arr_init(lam_array_t *arr)
{
    SUPER_INIT(arr, lam_array_cls.cls_parent);
    arr->arr_items = NULL;
    arr->arr_size = 0;
    arr->arr_length = 0;
}

void lam_arr_destroy(lam_array_t *arr)
{
    lam_arr_remove_all(arr);
    free(arr->arr_items);
    SUPER_DESTROY(arr, lam_array_cls.cls_parent);
}

bool lam_arr_init_with(lam_array_t *arr, size_t length)
{
    /* initializes array with fixed length.
    lam_arr_init() must have been called first. */
    if ( arr->arr_items )
    {
        lam_arr_remove_all(arr);
    }
    
    arr->arr_items = malloc(sizeof(lam_object_t *)*length);
    if ( arr->arr_items )
    {
        arr->arr_length = length;
        bzero(arr->arr_items, sizeof(lam_object_t *)*length);            
    }
    else
      return false;
    
    return true;
}

bool lam_arr_append_item(lam_array_t *arr, lam_object_t *item)
{
    if ( arr->arr_size == arr->arr_length )
    {
        arr->arr_items = (lam_object_t **)realloc(arr->arr_items,
                        sizeof(lam_object_t *)*(arr->arr_length + ARR_BLK_SZ));
        if ( arr->arr_items )
        {
            arr->arr_length += ARR_BLK_SZ;
        }
        else
        {
          return false;
        }
    }
    arr->arr_items[arr->arr_size++] = item;
    
    return true;
}

void lam_arr_remove_all(lam_array_t *arr)
{
    size_t      i;
    
    for ( i = 0; i < arr->arr_size; i++ )
    {
        OBJ_RELEASE(arr->arr_items[i]);
        arr->arr_items[i] = NULL;
    }
    arr->arr_size = 0;
}


void lam_arr_remove_item(lam_array_t *arr, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        arr->arr_items[index] = NULL;
    }
}

void lam_arr_set_item(lam_array_t *arr, lam_object_t *item, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        arr->arr_items[index] = item;
    }    
}

