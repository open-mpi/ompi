/*
 * $HEADER$
 */

#include "lam_config.h"
#include "lam/mem/free_list.h"
#include "lam/util/lam_log.h"
#include "lam/os/numa.h"
#include "lam/os/lam_system.h"
#include "lam/mem/mem_globals.h"

/* private list functions */
inline lam_flist_elt_t *lam_flr_request_elt(lam_free_list_t *flist, int pool_idx)
{
    lam_dbl_list_t      *seg_list = &(flist->fl_free_lists[pool_idx]->sgl_list);
    volatile lam_flist_elt_t *elt = lam_dbl_get_last(seg_list);
    
    if ( elt )
        lam_sgl_set_consec_fail(seg_list, 0);
    return elt;
}

void lam_frl_append(lam_free_list_t *flist, void *chunk, int pool_idx);

int lam_frl_create_more_elts(lam_free_list_t *flist, int pool_idx);

void *lam_frl_get_mem_chunk(lam_free_list_t *flist, int index, size_t *len, int *err);

int lam_frl_mem_pool_init(lam_free_list_t *flist, int nlists, long pages_per_list, ssize_t chunk_size,
                      size_t page_size, long min_pages_per_list,
                      long default_min_pages_per_list, long default_pages_per_list,
                      long max_pages_per_list, ssize_t max_mem_in_pool);

lam_class_info_t free_list_cls = {"lam_free_list_t", &lam_object_cls, 
    (class_init_t)lam_frl_init, (class_destroy_t)lam_frl_destroy};


void lam_frl_init(lam_free_list_t *flist)
{
    SUPER_INIT(flist, free_list_cls.cls_parent);
    lam_mtx_init(&flist->fl_lock);
    flist->fl_pool = 0;
    flist->fl_elt_cls = 0;
    flist->fl_description = 0;
    flist->fl_free_lists = 0;
    flist->fl_is_shared = 0;
    flist->fl_nlists = 0;
    flist->fl_elt_per_chunk = 0;
    flist->fl_elt_size = 0;
    flist->fl_retry_more_resources = 0;
    flist->fl_enforce_affinity = 0;
    flist->fl_affinity = 0;
    flist->fl_threshold_grow = 0;
    flist->fl_elt_out = 0;
    flist->fl_elt_max = 0;
    flist->fl_elt_sum = 0;
    flist->fl_nevents = 0;
    flist->fl_chunks_req= 0;
    flist->fl_chunks_returned = 0;
}


void lam_frl_destroy(lam_free_list_t *flist)
{
    int         i;
    
    OBJ_RELEASE(flist->fl_pool);
    for ( i = 0; i < flist->fl_nlists; i++ )
        OBJ_RELEASE(flist->fl_free_lists[i]);
    
    if ( flist->fl_affinity )
        free(flist->fl_affinity);
    
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
    
    SUPER_DESTROY(flist, free_list_cls.cls_parent);
}


int lam_frl_init_with(
        lam_free_list_t *flist, 
        int nlists,
        int pages_per_list,
        size_t chunk_size, 
        size_t page_size,
        size_t elt_size,
        int min_pages_per_list, 
        int max_pages_per_list,
        int max_consec_req_fail,
        const char *description,
        bool_t retry_for_more_resources,
        lam_affinity_t *affinity,
        bool_t enforce_affinity,
        lam_mem_pool_t *mem_pool)
{
    /* lam_frl_init must have been called prior to calling this function */
    size_t  max_mem_in_pool;
    size_t  initial_mem_per_list;
    long    max_mem_per_list;
    int     list, pool;
    int     err = LAM_SUCCESS;

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
        err = lam_frl_mem_pool_init(
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
        if (err != LAM_SUCCESS)
        {
            return err;
        }
    }
    
    /* reset pool chunk size */
    chunk_size = lam_mp_get_chunk_size(flist->fl_pool);
    
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
    flist->fl_free_lists = (lam_seg_list_t **)
                    malloc(sizeof(lam_seg_list_t *) *
                   flist->fl_nlists);
    if ( !flist->fl_free_lists )
    {
        lam_exit((-1, "Error: Out of memory\n"));
    }
 
    /* run constructors */
    for (list = 0; list < flist->fl_nlists; list++)
    {
        if ( flist->fl_is_shared )
        {
            /* process shared memory allocation */
            flist->fl_free_lists[list] =
            (lam_seg_list_t *)
            lam_fmp_get_mem_segment(&lam_per_proc_shmem_pools,
                sizeof(lam_seg_list_t), CACHE_ALIGNMENT, list);
        } 
        else
        {
            /* process private memory allocation */
            flist->fl_free_lists[list] =
                (lam_seg_list_t *)malloc(sizeof(lam_seg_list_t));
        }
        
        if (!flist->fl_free_lists[list])
            lam_exit((-1, "Error: Out of memory\n"));

        STATIC_INIT(flist->fl_free_lists[list], &seg_list_cls);
        
        lam_sgl_set_min_bytes_pushed(flist->fl_free_lists[list],
                                     initial_mem_per_list);
        lam_sgl_set_max_bytes_pushed(flist->fl_free_lists[list],
                                     max_mem_per_list);
        lam_sgl_set_max_consec_fail(flist->fl_free_lists[list],
                                    max_consec_req_fail);
    } /* end list loop */
    
    flist->fl_retry_more_resources = retry_for_more_resources;
    flist->fl_enforce_affinity = enforce_affinity;
    if ( enforce_affinity )
    {
        flist->fl_affinity = (affinity_t *)malloc(sizeof(affinity_t) *
                                    flist->fl_nlists);
        if ( !flist->fl_affinity )
            lam_exit((-1, "Error: Out of memory\n"));

        /* copy policies in */
        for ( pool = 0; pool < flist->fl_nlists; pool++ )
        {
            flist->fl_affinity[pool] = affinity[pool];
        }
    }


    // initialize locks for memory pool and individual list and link locks
    for ( pool = 0; pool < flist->fl_nlists; pool++ ) {
        
        // gain exclusive use of list
        if ( 1 == lam_sgl_lock_list(flist->fl_free_lists[pool]) ) {
            
            while ( lam_sgl_get_bytes_pushed(flist->fl_free_lists[pool])
                   < lam_sgl_get_min_bytes_pushed(flist->fl_free_lists[pool]) )
            {
                if (lam_frl_create_more_elts(flist, pool) != LAM_SUCCESS)
                {
                    lam_exit((-1, "Error: Setting up initial private "
                              "free list for %s.\n", flist->fl_description));
                }
            }
            
            lam_sgl_unlock_list(flist->fl_free_lists[pool]);
        }
        else
        {
            /* only 1 process should be initializing the list */
            lam_exit((-1, "Error: Setting up initial private free "
                      "list %d for %s.\n", pool, flist->fl_description));
        }
    }   
    
    return err;
    
}


int lam_frl_mem_pool_init(lam_free_list_t *flist,
                      int nlists, long pages_per_list, ssize_t chunk_size,
                      size_t page_size, long min_pages_per_list,
                      long default_min_pages_per_list, long default_pages_per_list,
                      long max_pages_per_list, ssize_t max_mem_in_pool)
{
    int         err = LAM_SUCCESS;
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
        to_alloc = sizeof(lam_mem_pool_t);
        flist->fl_pool =
            (lam_mem_pool_t *)lam_fmp_get_mem_segment(&lam_shmem_pools,
                                                      to_alloc, 
                                                      CACHE_ALIGNMENT, 0);
        if ( flist->fl_pool )
            STATIC_INIT(flist->fl_pool, &shmem_pool_cls);
    } else {
        /* process private memory allocation */
        CREATE_OBJECT(flist->fl_pool, lam_mem_pool_t, &mem_pool_cls);
    }

    err = lam_mp_init_with(
        flist->fl_pool, 
        mem_in_pool, 
        max_mem_in_pool,
        chunk_size, 
        page_size);
    return err;
}


void *lam_frl_get_mem_chunk(lam_free_list_t *flist, int index, size_t *len, int *err)
{
    void        *chunk = 0;
    uint64_t    sz_to_add;
    
    /* check to make sure that the amount to add to the list does not 
       exceed the amount allowed */
    sz_to_add = lam_mp_get_chunk_size(flist->fl_pool);
    
    /* need to add option to configure */
    if (OPT_MEMPROFILE)
    {
        flist->fl_chunks_req[index]++;
    }
    
    if (index >= flist->fl_nlists)
    {
        lam_err(("Error: Array out of bounds\n"));
        return chunk;
    }
        
    if ( lam_sgl_get_max_bytes_pushed(flist->fl_free_lists[index]) != -1 ) 
    {
        if (sz_to_add +
            lam_sgl_get_bytes_pushed(flist->fl_free_lists[index]) >
            lam_sgl_get_max_bytes_pushed(flist->fl_free_lists[index]) )
        {
            lam_sgl_inc_consec_fail(flist->fl_free_lists[index]); 
            if ( lam_sgl_get_consec_fail(flist->fl_free_lists[index]) >=
                lam_sgl_get_max_consec_fail(flist->fl_free_lists[index]) )
            {
                *err = LAM_ERR_OUT_OF_RESOURCE;
                lam_err(("Error: List out of memory in pool for %s\n",
                         flist->fl_description));
                return chunk;
            } else
                *err = LAM_ERR_TEMP_OUT_OF_RESOURCE;
            
            return chunk;
        }
    }
    // set len
    *len = sz_to_add;
    
    
    // get chunk of memory
    chunk = lam_mp_request_chunk(flist->fl_pool, index);
    if ( 0 == chunk )
    {
        // increment failure count
        lam_sgl_inc_consec_fail(flist->fl_free_lists[index]); 
        if ( lam_sgl_get_consec_fail(flist->fl_free_lists[index]) >=
             lam_sgl_get_max_consec_fail(flist->fl_free_lists[index]) )
        {
            *err = LAM_ERR_OUT_OF_RESOURCE;
            lam_err(("Error: List out of memory in pool for %s\n",
                     flist->fl_description));
            return chunk;
        } else
            *err = LAM_ERR_TEMP_OUT_OF_RESOURCE;
        
        return chunk;
    }
    
    /* set consecutive failure count to 0 - if we fail, we don't get 
       this far in the code. */
    lam_sgl_set_consec_fail(flist->fl_free_lists[index], 0);
    
    if (OPT_MEMPROFILE)
    {
        flist->fl_chunks_returned[index]++;
    }

    return chunk;
}



void lam_frl_append(lam_free_list_t *flist, void *chunk, int pool_idx)
{
    /* ASSERT: mp_chunk_sz >= fl_elt_per_chunk * fl_elt_size */
    // push items onto list 
    lam_sgl_append_elt_chunk(flist->fl_free_lists[pool_idx],
        chunk, lam_mp_get_chunk_size(flist->fl_pool),
        flist->fl_elt_per_chunk, flist->fl_elt_size);
}




int lam_frl_create_more_elts(lam_free_list_t *flist, int pool_idx)
{
    int         err = LAM_SUCCESS, desc;
    size_t      len_added;
    char        *current_loc;
    
    void *ptr = lam_frl_get_mem_chunk(flist, pool_idx, &len_added, &err);
    
    if (0 == ptr ) {
        lam_err(("Error: Can't get new elements for %s\n", 
                 flist->fl_description));
        return err;
    }
    
    // attach memory affinity
    if ( flist->fl_enforce_affinity )
    {
        if (!lam_set_affinity(ptr, len_added,
                         flist->fl_affinity[pool_idx]))
        {
            err = LAM_ERROR;
#ifdef _DEBUGQUEUES
            lam_err(("Error: Can't set memory policy (pool_idx=%d)\n",
                     pool_idx));
            return err;
#endif                          /* _DEBUGQUEUES */
        }
    }
    
    /* Construct new descriptors using placement new */
    current_loc = (char *) ptr;
    for (desc = 0; desc < flist->fl_elt_per_chunk; desc++)
    {
        STATIC_INIT(*(lam_flist_elt_t *)current_loc, flist->fl_elt_cls);
        lam_fle_set_idx((lam_flist_elt_t *)current_loc, pool_idx);
        current_loc += flist->fl_elt_size;
    }
    
    /* push chunk of memory onto the list */
    lam_frl_append(flist, ptr, pool_idx);
    
    return err;
}




lam_flist_elt_t *lam_frl_get_elt(lam_free_list_t *flist, int index, int *error)
{
    int         error;
    volatile    lam_flist_elt_t *elem = 0;
    
    elem = lam_flr_request_elt(flist, index);
    
    if ( elem ) 
    {
        error = LAM_SUCCESS;
    } 
    else if ( lam_sgl_get_consec_fail(&(flist->fl_free_lists[index]->sgl_list))
               < flist->fl_threshold_grow ) 
    {
        error = LAM_ERR_TEMP_OUT_OF_RESOURCE;
    } 
    else 
    {
        error = LAM_SUCCESS;
        while ( (LAM_SUCCESS) && (0 == elem) &&
                (flist->fl_retry_more_resources) )
        {
            error = lam_frl_create_more_elts(flist, index);
            /* get element if managed to add resources to the list */
            if ( LAM_SUCCESS == error )
            {
                elem = lam_flr_request_elt(flist, index);
            }            
        }

        if ( (LAM_ERR_OUT_OF_RESOURCE == error)
             || (LAM_ERR_FATAL == error) )
        {
            return 0;
        }
    }
    if ( OPT_MEMPROFILE )
    {
        flist->fl_elt_out[index]++;
        flist->fl_elt_sum[index] += flist->fl_elt_out[index];
        flist->fl_nevents[index]++;
        if (flist->fl_elt_max[index] < flist->fl_elt_out[index])
        {
            flist->fl_elt_max[index] = flist->fl_elt_out[index];
        }
    }
    
    return elem;
}

int lam_frl_return_elt(lam_free_list_t *flist, int index, lam_flist_elt_t *item)
{
    mb();
    lam_dbl_append(&(flist->fl_free_lists[index]->sgl_list), item);
    mb();
    
    if ( OPT_MEMPROFILE ) {
        flist->fl_elt_out[index]--;
    }
    
    return LAM_SUCCESS;
}


