/*
 * $HEADER$
 */

#ifndef LAM_FREE_LIST_H
#define LAM_FREE_LIST_H

#include "lam_config.h"
#include "lam/lfc/list.h"
#include "lam/constants.h"
#include "lam/mem/seg_list.h"
#include "lam/mem/mem_pool.h"

extern lam_class_info_t lam_free_lists_cls;


struct lam_free_list_t
{
    lam_list_t super;
    int fl_max_to_alloc;
    int fl_num_allocated;
    int fl_num_per_alloc;
    lam_allocator_t fl_allocator;
};
typedef struct lam_free_list_t lam_free_list_t;


void lam_free_list_init(lam_free_list_t *flist);
void lam_free_list_destroy(lam_free_list_t *flist);


/* lam_free_list_init() must have been called prior to calling this function */
int lam_free_list_init_with(
    lam_free_list_t *flist, 
    size_t element_size,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    lam_allocator_t);

static inline lam_list_item_t *lam_free_list_get(lam_free_list_t * list, int *rc)
{
    return NULL;
}

static inline int lam_free_list_return(lam_free_list_t *list, lam_list_item_t *rc)
{
    return LAM_ERROR;
}

#endif 

