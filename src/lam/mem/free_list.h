/*
 * $HEADER$
 */

#ifndef LAM_FREE_LIST
#define LAM_FREE_LIST

#include "lam_config.h"
#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"
#include "lam/mem/seg_list.h"
#include "lam/mem/mem_pool.h"

/*
 *  Free list element interface
 *  Any item that goes into the free list must
 *  inherit from this class.
 */

typedef struct lam_flist_elt
{
    lam_list_item_t     super;
    int                 fle_pool_idx;
} lam_flist_elt_t;

void lam_flist_elt_init(lam_flist_elt_t*);
void lam_flist_elt_destroy(lam_flist_elt_t*);

#define lam_fle_get_idx(elt) (elt)->fle_pool_idx
#define lam_fle_set_idx(elt, idx) ((elt)->fle_pool_idx = idx)

/*
 * Memory affinity is almost certainly an int everywhere, but let's
 * make it a typedef in case we need to make it OS dependenent
 * sometime...
 */

typedef int lam_affinity_t;

typedef struct lam_free_list
{
    lam_object_t        super;
    int                 fl_is_shared;
    lam_mem_pool_t     *fl_pool;
    const char         *fl_description;
    int                 fl_nlists;
    int                 fl_elt_per_chunk;
    size_t              fl_elt_size;
    lam_seg_list_t    **fl_free_lists;
    int                 fl_retry_more_resources;
    int                 fl_enforce_affinity;
    lam_affinity_t     *fl_affinity;            /* array of lam_affinity_t */
    int                 fl_threshold_grow;
    lam_class_info_t   *fl_elt_cls;        /* this will be used to create new free list elements. */
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
} lam_free_list_t;

extern lam_class_info_t free_list_cls;

void lam_frl_init(lam_free_list_t *flist);
void lam_frl_destroy(lam_free_list_t *flist);

/* lam_frl_init must have been called prior to calling this function */
int lam_frl_init_with(lam_free_list_t *flist, 
                  int nlists,
                  int pages_per_list,
                  size_t chunk_size, 
                  size_t page_size,
                  size_t element_size,
                  int min_pages_per_list, 
                  int max_pages_per_list,
                  int max_consec_req_fail,
                  const char *description,
                  bool retry_for_more_resources,
                  lam_affinity_t *affinity,
                  bool enforce_affinity,
                  lam_mem_pool_t *pool);


lam_flist_elt_t *lam_frl_get_elt(lam_free_list_t *flist, int index, int *error);

int lam_frl_return_elt(lam_free_list_t *flist, int index, lam_flist_elt_t *item);

/*
 *      Accessor functions
 */

int lam_frl_get_thresh_grow(lam_free_list_t *flist);
void lam_frl_set_thresh_grow(lam_free_list_t *flist, int to_grow);

#endif 


