/*
 * $HEADER$
 */

#ifndef LAM_FREE_LIST_H
#define LAM_FREE_LIST_H

#include "lam_config.h"
#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"
#include "lam/mem/seg_list.h"
#include "lam/mem/mem_pool.h"

extern lam_class_info_t lam_free_lists_cls;


struct lam_free_list_t
{
    lam_object_t        super;
    int                 fl_is_shared;
    lam_mem_pool_t     *fl_pool;
    const char         *fl_description;
    int                 fl_elt_per_chunk;
    size_t              fl_elt_size;
    lam_seg_list_t     *fl_free_list;
    int                 fl_retry_more_resources;
    int                 fl_enforce_affinity;
    int                 fl_threshold_grow;
    lam_class_info_t   *fl_elt_cls;   /* this will be used to create new free list elements. */
    lam_mutex_t         fl_lock;
    
    /* for mem profiling */
    int           *fl_elt_out;
    int           *fl_elt_max;
    int           *fl_elt_sum;
    int           *fl_nevents;
#if LAM_ENABLE_DEBUG
    int           *fl_chunks_req;
    int           *fl_chunks_returned;
#endif
};
typedef struct lam_free_list_t lam_free_list_t;



void lam_free_list_init(lam_free_list_t *flist);
void lam_free_list_destroy(lam_free_list_t *flist);

/* lam_free_list_init() must have been called prior to calling this function */
int lam_free_list_init_with(
        lam_free_list_t *flist, 
        size_t element_size,
        int min_pages, 
        int max_pages,
        int num_pages_per_alloc,
        lam_mem_pool_t *pool);

lam_list_item_t *lam_free_list_get(lam_free_list_t *, int *);
int lam_free_list_return(lam_free_list_t *, lam_list_item_t *);

#endif 

