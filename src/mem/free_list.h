/*
 * $HEADER$
 */

#ifndef LAM_FREE_LIST_H
#define LAM_FREE_LIST_H

#include "lam_config.h"
#include "lfc/lam_list.h"
#include "include/constants.h"
#include "mem/seg_list.h"
#include "mem/mem_pool.h"

extern lam_class_t lam_free_list_t_class;


struct lam_free_list_t
{
    lam_list_t super;
    int fl_max_to_alloc;
    int fl_num_allocated;
    int fl_num_per_alloc;
    size_t fl_elem_size;
    lam_class_t* fl_elem_class;
    lam_allocator_t* fl_allocator;
    lam_mutex_t fl_lock;
};
typedef struct lam_free_list_t lam_free_list_t;


int lam_free_list_init(
    lam_free_list_t *flist, 
    size_t element_size,
    lam_class_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    lam_allocator_t*);

int lam_free_list_grow(lam_free_list_t* flist, size_t num_elements);
    

#define LAM_FREE_LIST_GET(fl, item, rc) \
{ \
    if(lam_using_threads()) { \
        lam_mutex_lock(&((fl)->fl_lock)); \
        item = lam_list_remove_first(&((fl)->super)); \
        if(NULL == item) { \
            lam_free_list_grow((fl), (fl)->fl_num_per_alloc); \
            item = lam_list_remove_first(&((fl)->super)); \
        } \
        lam_mutex_unlock(&((fl)->fl_lock)); \
    } else { \
        item = lam_list_remove_first(&((fl)->super)); \
        if(NULL == item) { \
            lam_free_list_grow((fl), (fl)->fl_num_per_alloc); \
            item = lam_list_remove_first(&((fl)->super)); \
        } \
    }  \
    rc = (NULL == item) ?  LAM_ERR_TEMP_OUT_OF_RESOURCE : LAM_SUCCESS; \
} 


#define LAM_FREE_LIST_RETURN(fl, item) \
    THREAD_SCOPED_LOCK(&((fl)->fl_lock), lam_list_append(&((fl)->super), (item))); 

#endif 

