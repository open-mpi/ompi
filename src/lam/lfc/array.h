/*
 * $HEADER$
 */

#ifndef LAM_ARRAY_H
#define LAM_ARRAY_H

#include "lam_config.h"
#include "include/lam_types.h"
#include "lam/lfc/object.h"

/*
 *
 *      Available Classes
 *
 */

extern lam_class_info_t    lam_array_cls;


/*
 *
 *      Arrray interface
 *
 */

typedef int     (*lam_arr_cmp_fn)(lam_object_t *, lam_object_t *);

typedef struct lam_array
{
    lam_object_t    super;
    lam_object_t    **arr_items;
    size_t          arr_length;
    size_t          arr_size;
} lam_array_t;


void lam_arr_init(lam_array_t *arr);
void lam_arr_destroy(lam_array_t *arr);

/* initializes array with fixed length.
 * lam_arr_init() must have been called first.
 */
lam_bool_t lam_arr_init_with(lam_array_t *arr, size_t length);

lam_bool_t lam_arr_append_item(lam_array_t *arr, lam_object_t *item);

lam_object_t *lam_arr_get_item(lam_array_t *arr, int index);
inline lam_object_t *lam_arr_get_item(lam_array_t *arr, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        return arr->arr_items[index];
    }
}

size_t lam_arr_get_size(lam_array_t *arr);
inline size_t lam_arr_get_size(lam_array_t *arr)
{
    return arr->arr_size;
}

int lam_arr_index_of_item_matching(lam_array_t *arr, lam_object_t *item,
                                   lam_arr_cmp_fn cmp_fn);

void lam_arr_remove_all(lam_array_t *arr);

void lam_arr_remove_item(lam_array_t *arr, int index);

void lam_arr_remove_item_matching(lam_array_t *arr, lam_object_t *item,
                                  lam_arr_cmp_fn cmp_fn);

void lam_arr_set_item(lam_array_t *arr, lam_object_t *item, int index);

lam_object_t **lam_arr_get_c_array(lam_array_t *arr, size_t *size);
inline lam_object_t **lam_arr_get_c_array(lam_array_t *arr, size_t *size)
{
    *size = arr->arr_size;
    return arr->arr_items;
}


#endif  /* LAM_ARRAY_H */


