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
    size_t fl_elem_size;
    lam_class_info_t* fl_elem_class;
    lam_allocator_t* fl_allocator;
    lam_mutex_t fl_lock;
};
typedef struct lam_free_list_t lam_free_list_t;


void lam_free_list_init(lam_free_list_t *flist);
void lam_free_list_destroy(lam_free_list_t *flist);
int  lam_free_list_grow(lam_free_list_t* flist, size_t num_elements);


int lam_free_list_init_with(
    lam_free_list_t *flist, 
    size_t element_size,
    lam_class_info_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    lam_allocator_t*);
    

static inline lam_list_item_t *lam_free_list_get(lam_free_list_t * fl, int *rc)
{
    lam_list_item_t* item;
    THREAD_LOCK(&fl->fl_lock);
    item = lam_list_remove_first(&fl->super);
    if(NULL == item) {
        lam_free_list_grow(fl, fl->fl_num_per_alloc);
        item = lam_list_remove_first(&fl->super);
    }
    THREAD_UNLOCK(&fl->fl_lock);
    *rc = (NULL != item) ? LAM_SUCCESS : LAM_ERR_TEMP_OUT_OF_RESOURCE;
    return item;
}

static inline int lam_free_list_return(lam_free_list_t *fl, lam_list_item_t *item)
{
    THREAD_LOCK(&fl->fl_lock);
    lam_list_append(&fl->super, item);
    THREAD_UNLOCK(&fl->fl_lock);
    return LAM_SUCCESS;
}

#endif 

