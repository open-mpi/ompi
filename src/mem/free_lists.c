/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "mem/free_lists.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "os/numa.h"
#include "os/ompi_system.h"
#include "mem/mem_globals.h"

#ifndef ROB_HASNT_FINISHED_THIS_YET
#define ROB_HASNT_FINISHED_THIS_YET 0
#endif

/* private list functions */

#if ROB_HASNT_FINISHED_THIS_YET
static ompi_list_item_t *ompi_free_lists_request_elt(ompi_free_lists_t *flist, 
                                            int pool_idx);
#endif

static void ompi_free_lists_append(ompi_free_lists_t *flist, void *chunk, int pool_idx);

static int ompi_free_lists_create_more_elts(ompi_free_lists_t *flist, int pool_idx);

static void *ompi_free_lists_get_mem_chunk(ompi_free_lists_t *flist, int index, size_t *len, int *err);

static int ompi_free_lists_mem_pool_construct(ompi_free_lists_t *flist, int nlists, long pages_per_list, ssize_t chunk_size,
                      size_t page_size, long min_pages_per_list,
                      long default_min_pages_per_list, long default_pages_per_list,
                      long max_pages_per_list, ssize_t max_mem_in_pool);

ompi_class_t ompi_free_lists_t_class = {
    "ompi_free_lists_t",
    OBJ_CLASS(ompi_object_t), 
    (ompi_construct_t) ompi_free_lists_construct,
    (ompi_destruct_t) ompi_free_lists_destruct
};


void ompi_free_lists_construct(ompi_free_lists_t *flist)
{
    OBJ_CONSTRUCT(&flist->fl_lock, ompi_mutex_t);
    flist->fl_pool = NULL;
    flist->fl_elt_cls = NULL;
    flist->fl_description = NULL;
    flist->fl_free_lists = NULL;
    flist->fl_is_shared = 0;
    flist->fl_nlists = 0;
    flist->fl_elt_per_chunk = 0;
    flist->fl_elt_size = 0;
    flist->fl_retry_more_resources = 0;
    flist->fl_enforce_affinity = 0;
    flist->fl_affinity = NULL;
    flist->fl_threshold_grow = 0;

#if OMPI_ENABLE_MEM_PROFILE
    flist->fl_elt_out = NULL;
    flist->fl_elt_max = NULL;
    flist->fl_elt_sum = NULL;
    flist->fl_nevents = NULL;
    flist->fl_chunks_req = NULL;
    flist->fl_chunks_returned = NULL;
#endif  /* OMPI_ENABLE_MEM_PROFILE */
}


void ompi_free_lists_destruct(ompi_free_lists_t *flist)
{
    int         i;
    
    OBJ_RELEASE(flist->fl_pool);
    for ( i = 0; i < flist->fl_nlists; i++ )
        OBJ_RELEASE(flist->fl_free_lists[i]);
    
    if ( flist->fl_affinity )
        free(flist->fl_affinity);

#if OMPI_ENABLE_MEM_PROFILE    
    if ( flist->fl_elt_out )
        free(flist->fl_elt_out);
    
    if ( flist->fl_elt_max )
        free(flist->fl_elt_max);
    
    if ( flist->fl_elt_sum )
        free(flist->fl_elt_sum);
    
    if ( flist->fl_nevents )
        free(flist->fl_nevents);
    
    if ( flist->fl_chunks_req )
        free(flist->fl_chunks_req);
    
    if ( flist->fl_chunks_returned )
        free(flist->fl_chunks_returned);
#endif  /* OMPI_ENABLE_MEM_PROFILE */
}


int ompi_free_lists_construct_with(
        ompi_free_lists_t *flist, 
        int nlists,
        int pages_per_list,
        size_t chunk_size, 
        size_t page_size,
        size_t elt_size,
        int min_pages_per_list, 
        int max_pages_per_list,
        int max_consec_req_fail,
        const char *description,
        bool retry_for_more_resources,
        ompi_affinity_t *affinity,
        bool enforce_affinity,
        ompi_mem_pool_t *mem_pool)
{
    /* ompi_free_lists_construct must have been called prior to calling this function */
    size_t  max_mem_in_pool;
    size_t  initial_mem_per_list;
    long    max_mem_per_list;
    int     list, pool;
    int     err = OMPI_SUCCESS;

    flist->fl_description = description;
    flist->fl_nlists = nlists;
    
    /* set up the memory pool */
    if ( mem_pool )
    {
        flist->fl_pool = mem_pool;
        OBJ_RETAIN(flist->fl_pool);
    } 
    else
    {
        /* instantiate memory pool */
        max_mem_in_pool = max_pages_per_list * page_size;
        err = ompi_free_lists_mem_pool_construct(
            flist,
            nlists, 
            pages_per_list, 
            chunk_size,
            page_size, 
            min_pages_per_list,
            min_pages_per_list, 
            pages_per_list,
            max_pages_per_list, 
            max_mem_in_pool);
        if (err != OMPI_SUCCESS)
        {
            return err;
        }
    }
    
    /* reset pool chunk size */
    chunk_size = ompi_mp_get_chunk_size(flist->fl_pool);
    
    /* Number of elements per chunk */
    flist->fl_elt_per_chunk = chunk_size / elt_size;
    
    initial_mem_per_list = min_pages_per_list * page_size;
    
    /* adjust initial_mem_per_list to increments of chunk_size */
    if ( initial_mem_per_list < chunk_size )
    {
        min_pages_per_list = (((chunk_size - 1) / page_size) + 1);
        initial_mem_per_list = min_pages_per_list * page_size;
    }
    
    /* determine upper limit on number of pages in a given list */
    if ( (max_pages_per_list != -1) && (max_pages_per_list < min_pages_per_list) )
        max_pages_per_list = min_pages_per_list;
    
    if (max_pages_per_list == -1)
        max_mem_per_list = -1;
    else
        max_mem_per_list = max_pages_per_list * page_size;
    
    /* initialize empty lists of available descriptors */
    flist->fl_free_lists = (ompi_seg_list_t **)
                    malloc(sizeof(ompi_seg_list_t *) *
                   flist->fl_nlists);
    if ( !flist->fl_free_lists )
    {
      ompi_abort(1, "Error: Out of memory");
    }
 
    /* run constructors */
    for (list = 0; list < flist->fl_nlists; list++)
    {
        if ( flist->fl_is_shared )
        {
            /* process shared memory allocation */
            flist->fl_free_lists[list] =
            (ompi_seg_list_t *)
            ompi_fmp_get_mem_segment(&ompi_per_proc_shmem_pools,
                sizeof(ompi_seg_list_t), CACHE_ALIGNMENT, list);
        } 
        else
        {
            /* process private memory allocation */
            flist->fl_free_lists[list] =
                (ompi_seg_list_t *)malloc(sizeof(ompi_seg_list_t));
        }
        
        if (!flist->fl_free_lists[list]) {
          ompi_abort(1, "Error: Out of memory");
        }

        OBJ_CONSTRUCT(&flist->fl_free_lists[list], ompi_seg_list_t);
        
        ompi_sgl_set_min_bytes_pushed(flist->fl_free_lists[list],
                                     initial_mem_per_list);
        ompi_sgl_set_max_bytes_pushed(flist->fl_free_lists[list],
                                     max_mem_per_list);
        ompi_sgl_set_max_consec_fail(flist->fl_free_lists[list],
                                    max_consec_req_fail);
    } /* end list loop */
    
    flist->fl_retry_more_resources = retry_for_more_resources;
    flist->fl_enforce_affinity = enforce_affinity;
    if ( enforce_affinity )
    {
        flist->fl_affinity = (affinity_t *)malloc(sizeof(affinity_t) *
                                    flist->fl_nlists);
        if ( !flist->fl_affinity ) {
          ompi_abort(1, "Error: Out of memory");
        }

        /* copy policies in */
        for ( pool = 0; pool < flist->fl_nlists; pool++ )
        {
            flist->fl_affinity[pool] = affinity[pool];
        }
    }


    /* initialize locks for memory pool and individual list and link locks */
    for ( pool = 0; pool < flist->fl_nlists; pool++ ) {
        
        /* gain exclusive use of list */
        if ( 1 == ompi_sgl_lock_list(flist->fl_free_lists[pool]) ) {
            
            while ( ompi_sgl_get_bytes_pushed(flist->fl_free_lists[pool])
                   < ompi_sgl_get_min_bytes_pushed(flist->fl_free_lists[pool]) )
            {
                if (ompi_free_lists_create_more_elts(flist, pool) != OMPI_SUCCESS)
                {
                  ompi_abort(1, "Error: Setting up initial private "
                            "free list for %s.\n", flist->fl_description);
                }
            }
            
            ompi_sgl_unlock_list(flist->fl_free_lists[pool]);
        }
        else
        {
            /* only 1 process should be initializing the list */
            ompi_abort(1, "Error: Setting up initial private free "
                      "list %d for %s.\n", pool, flist->fl_description);
        }
    }   
    
    return err;
    
}


static int ompi_free_lists_mem_pool_construct(ompi_free_lists_t *flist,
                      int nlists, long pages_per_list, ssize_t chunk_size,
                      size_t page_size, long min_pages_per_list,
                      long default_min_pages_per_list, long default_pages_per_list,
                      long max_pages_per_list, ssize_t max_mem_in_pool)
{
    int         err = OMPI_SUCCESS;
    long        total_pgs_to_alloc;
    ssize_t     mem_in_pool;
    size_t      to_alloc;
    
    /* set chunksize - multiple of page size */
    chunk_size =
        ((((chunk_size - 1) / page_size) + 1) * page_size);
    
    /* determine number how much memory to allocate */
    if ( pages_per_list == -1 ) {
        /* minimum size is  defaultNPagesPerList*number of local procs */
        total_pgs_to_alloc = default_pages_per_list * nlists;
    } else {
        total_pgs_to_alloc = pages_per_list * nlists;
    }
    
    mem_in_pool = total_pgs_to_alloc * page_size;
    
    /* Initialize memory pool */
    if ( flist->fl_is_shared ) {
        /* shared memory allocation */
        to_alloc = sizeof(ompi_mem_pool_t);
        flist->fl_pool =
            (ompi_mem_pool_t *)ompi_fmp_get_mem_segment(&ompi_shmem_pools,
                                                      to_alloc, 
                                                      CACHE_ALIGNMENT, 0);
        if ( flist->fl_pool ) {
            OBJ_CONSTRUCT(&flist->fl_pool, shmem_pool_t);
        }
    } else {
        /* process private memory allocation */
        flist->fl_pool = OBJ_NEW(ompi_mem_pool_t);
    }

    err = ompi_mp_construct_with(
        flist->fl_pool, 
        mem_in_pool, 
        max_mem_in_pool,
        chunk_size, 
        page_size);
    return err;
}


static void *ompi_free_lists_get_mem_chunk(ompi_free_lists_t *flist, int index, size_t *len, int *err)
{
    void        *chunk = 0;
    uint64_t    sz_to_add;
    
    /* check to make sure that the amount to add to the list does not 
       exceed the amount allowed */
    sz_to_add = ompi_mp_get_chunk_size(flist->fl_pool);

#if OMPI_ENABLE_MEM_PROFILE
    flist->fl_chunks_req[index]++;
#endif
    
    if (index >= flist->fl_nlists)
    {
      ompi_output(0, "Error: Array out of bounds");
      return chunk;
    }
        
    if ( ompi_sgl_get_max_bytes_pushed(flist->fl_free_lists[index]) != -1 ) 
    {
        if (sz_to_add +
            ompi_sgl_get_bytes_pushed(flist->fl_free_lists[index]) >
            ompi_sgl_get_max_bytes_pushed(flist->fl_free_lists[index]) )
        {
            ompi_sgl_inc_consec_fail(flist->fl_free_lists[index]); 
            if ( ompi_sgl_get_consec_fail(flist->fl_free_lists[index]) >=
                ompi_sgl_get_max_consec_fail(flist->fl_free_lists[index]) )
            {
                *err = OMPI_ERR_OUT_OF_RESOURCE;
                ompi_output(0, "Error: List out of memory in pool for %s",
                           flist->fl_description);
                return chunk;
            } else
                *err = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            
            return chunk;
        }
    }
    /* set len */
    *len = sz_to_add;
    
    
    /* get chunk of memory */
    chunk = ompi_mp_request_chunk(flist->fl_pool, index);
    if ( 0 == chunk )
    {
        /* increment failure count */
        ompi_sgl_inc_consec_fail(flist->fl_free_lists[index]); 
        if ( ompi_sgl_get_consec_fail(flist->fl_free_lists[index]) >=
             ompi_sgl_get_max_consec_fail(flist->fl_free_lists[index]) )
        {
            *err = OMPI_ERR_OUT_OF_RESOURCE;
            ompi_output(0, "Error: List out of memory in pool for %s\n",
                       flist->fl_description);
            return chunk;
        } else
            *err = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        
        return chunk;
    }
    
    /* set consecutive failure count to 0 - if we fail, we don't get 
       this far in the code. */
    ompi_sgl_set_consec_fail(flist->fl_free_lists[index], 0);
    
#if OMPI_ENABLE_MEM_PROFILE
    flist->fl_chunks_returned[index]++;
#endif

    return chunk;
}



#if ROB_HASNT_FINISHED_THIS_YET
static ompi_list_item_t *ompi_free_lists_request_elt(ompi_free_lists_t *flist, int pool_idx)
{
    ompi_dbl_list_t      *seg_list = &(flist->fl_free_lists[pool_idx]->sgl_list);
    volatile ompi_list_item_t *elt = ompi_dbl_get_last(seg_list);
    
    if ( elt )
        ompi_sgl_set_consec_fail(seg_list, 0);
    return elt;
}
#endif


static void ompi_free_lists_append(ompi_free_lists_t *flist, void *chunk, int pool_idx)
{
    /* ASSERT: mp_chunk_sz >= fl_elt_per_chunk * fl_elt_size */
    /* push items onto list  */
    ompi_sgl_append_elt_chunk(flist->fl_free_lists[pool_idx],
        chunk, ompi_mp_get_chunk_size(flist->fl_pool),
        flist->fl_elt_per_chunk, flist->fl_elt_size);
}




static int ompi_free_lists_create_more_elts(ompi_free_lists_t *flist, int pool_idx)
{
    int         err = OMPI_SUCCESS, desc;
    size_t      len_added;
    char        *current_loc;
    
    void *ptr = ompi_free_lists_get_mem_chunk(flist, pool_idx, &len_added, &err);
    
    if (0 == ptr ) {
      ompi_output(0, "Error: Can't get new elements for %s\n", 
                 flist->fl_description);
        return err;
    }
    
    /* attach memory affinity */
    if ( flist->fl_enforce_affinity )
    {
        if (!ompi_set_affinity(ptr, len_added,
                         flist->fl_affinity[pool_idx]))
        {
            err = OMPI_ERROR;
#ifdef _DEBUGQUEUES
            ompi_err(("Error: Can't set memory policy (pool_idx=%d)\n",
                     pool_idx));
            return err;
#endif                          /* _DEBUGQUEUES */
        }
    }
    
    /* Construct new descriptors using placement new */
    current_loc = (char *) ptr;
    for (desc = 0; desc < flist->fl_elt_per_chunk; desc++)
    {
        OBJ_CONSTRUCT_INTERNAL(current_loc, flist->fl_elt_cls);
        current_loc += flist->fl_elt_size;
    }
    
    /* push chunk of memory onto the list */
    ompi_free_lists_append(flist, ptr, pool_idx);
    
    return err;
}




ompi_list_item_t *ompi_free_lists_get_elt(ompi_free_lists_t *flist, int index, int *error)
{
#if ROB_HASNT_FINISHED_THIS_YET
    int         error;
    volatile    ompi_list_item_t *elem = NULL;
    
    elem = ompi_free_lists_request_elt(flist, index);
    
    if ( elem ) 
    {
        error = OMPI_SUCCESS;
    } 
    else if ( ompi_sgl_get_consec_fail(&(flist->fl_free_lists[index]->sgl_list))
               < flist->fl_threshold_grow ) 
    {
        error = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    } 
    else 
    {
        error = OMPI_SUCCESS;
        while ( (OMPI_SUCCESS) && (0 == elem) &&
                (flist->fl_retry_more_resources) )
        {
            error = ompi_free_lists_create_more_elts(flist, index);
            /* get element if managed to add resources to the list */
            if ( OMPI_SUCCESS == error )
            {
                elem = ompi_free_lists_request_elt(flist, index);
            }            
        }

        if ( (OMPI_ERR_OUT_OF_RESOURCE == error)
             || (OMPI_ERR_FATAL == error) )
        {
            return 0;
        }
    }
#if OMPI_ENABLE_MEM_PROFILE
    flist->fl_elt_out[index]++;
    flist->fl_elt_sum[index] += flist->fl_elt_out[index];
    flist->fl_nevents[index]++;
    if (flist->fl_elt_max[index] < flist->fl_elt_out[index])
    {
        flist->fl_elt_max[index] = flist->fl_elt_out[index];
    }
#endif
    
    return elem;
#else
    return NULL;
#endif
}

int ompi_free_lists_return_elt(ompi_free_lists_t *flist, int index, ompi_list_item_t *item)
{
#if ROB_HASNT_FINISHED_THIS_YET
    mb();
    ompi_dbl_append(&(flist->fl_free_lists[index]->sgl_list), item);
    mb();
    
#if OMPI_ENABLE_MEM_PROFILE
    flist->fl_elt_out[index]--;
#endif
    
    return OMPI_SUCCESS;
#else
    return OMPI_ERROR;
#endif
}


