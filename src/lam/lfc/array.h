/*
 * $HEADER$
 */

#ifndef LAM_ARRAY_H
#define LAM_ARRAY_H

#include "lam_config.h"
#include "lam/types.h"
#include "lam/lfc/lam_object.h"

/*
 *
 *      Available Classes
 *
 */

extern lam_class_info_t    lam_array_t_class_info;


/*
 *
 *      Array interface
 *
 */

typedef int     (*lam_arr_cmp_fn)(lam_object_t *, lam_object_t *);

struct lam_array_t
{
    lam_object_t    super;
    lam_object_t    **arr_items;
    size_t          arr_length;
    size_t          arr_size;
};
typedef struct lam_array_t lam_array_t;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  void lam_arr_construct(lam_array_t *arr);
  void lam_arr_destruct(lam_array_t *arr);

  /* initializes array with fixed length.
   * lam_arr_construct() must have been called first.
   */
  bool lam_arr_construct_with(lam_array_t *arr, size_t length);
  
  bool lam_arr_append_item(lam_array_t *arr, lam_object_t *item);
  
  int lam_arr_index_of_item_matching(lam_array_t *arr, lam_object_t *item,
                                     lam_arr_cmp_fn cmp_fn);
  
  void lam_arr_remove_all(lam_array_t *arr);
  
  void lam_arr_remove_item(lam_array_t *arr, int index);
  
  void lam_arr_remove_item_matching(lam_array_t *arr, lam_object_t *item,
                                    lam_arr_cmp_fn cmp_fn);
  
  void lam_arr_set_item(lam_array_t *arr, lam_object_t *item, int index);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

static inline lam_object_t *lam_arr_get_item(lam_array_t *arr, size_t index);
static inline lam_object_t *lam_arr_get_item(lam_array_t *arr, size_t index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        return arr->arr_items[index];
    }
    return NULL;
}

static inline size_t lam_arr_get_size(lam_array_t *arr);
static inline size_t lam_arr_get_size(lam_array_t *arr)
{
    return arr->arr_size;
}

static inline lam_object_t **lam_arr_get_c_array(lam_array_t *arr, 
                                                size_t *size);
static inline lam_object_t **lam_arr_get_c_array(lam_array_t *arr, 
                                                 size_t *size)
{
    *size = arr->arr_size;
    return arr->arr_items;
}


#endif  /* LAM_ARRAY_H */


