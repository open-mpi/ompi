/*
 * $HEADER$
 */

#ifndef OMPI_FREE_LISTS_H
#define OMPI_FREE_LISTS_H

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "mem/seg_list.h"
#include "mem/mem_pool.h"

/*
 * Memory affinity is almost certainly an int everywhere, but let's
 * make it a typedef in case we need to make it OS dependenent
 * sometime...
 */

typedef int ompi_affinity_t;

struct ompi_free_lists_t
{
    ompi_object_t        super;
    int                 fl_is_shared;
    ompi_mem_pool_t     *fl_pool;
    const char         *fl_description;
    int                 fl_nlists;
    int                 fl_elt_per_chunk;
    size_t              fl_elt_size;
    ompi_seg_list_t    **fl_free_lists;
    int                 fl_retry_more_resources;
    int                 fl_enforce_affinity;
    ompi_affinity_t     *fl_affinity;            /* array of ompi_affinity_t */
    int                 fl_threshold_grow;
    ompi_class_t   *fl_elt_cls;   /* this will be used to create new free list elements. */
    ompi_mutex_t         fl_lock;
    
#if OMPI_ENABLE_MEM_PROFILE
    /* for mem profiling */
    int           *fl_elt_out;
    int           *fl_elt_max;
    int           *fl_elt_sum;
    int           *fl_nevents;
    int           *fl_chunks_req;
    int           *fl_chunks_returned;
#endif  /* OMPI_ENABLE_MEM_PROFILE */
};
typedef struct ompi_free_lists_t ompi_free_lists_t;


extern ompi_class_t ompi_free_lists_t_class;

void ompi_free_lists_construct(ompi_free_lists_t *flist);
void ompi_free_lists_destruct(ompi_free_lists_t *flist);

/* ompi_frl_construct must have been called prior to calling this function */
int ompi_free_lists_construct_with(ompi_free_lists_t *flist, 
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
                  ompi_affinity_t *affinity,
                  bool enforce_affinity,
                  ompi_mem_pool_t *pool);


ompi_list_item_t *ompi_free_lists_get_elt(ompi_free_lists_t *flist, int index, int *error);

int ompi_free_lists_return_elt(ompi_free_lists_t *flist, int index, ompi_list_item_t *item);

/*
 *      Accessor functions
 */

int  ompi_free_lists_get_thresh_grow(ompi_free_lists_t *flist);
void ompi_free_lists_set_thresh_grow(ompi_free_lists_t *flist, int to_grow);

#endif 


