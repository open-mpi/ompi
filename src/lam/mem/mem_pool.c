/*
 * $HEADER$
 */

#include "lam_config.h"

#include <string.h>
#include <sys/errno.h>
#include <unistd.h>

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/mem/mem_pool.h"
#include "lam/mem/sharedmem_util.h"
#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "lam/os/numa.h"

lam_class_t lam_mem_pool_t_class = {
    "lam_mem_pool_t",
    OBJ_CLASS(lam_object_t), 
    (lam_construct_t) lam_mp_construct,
    (lam_destruct_t) lam_mp_destruct
};

/* process-shared mem pool class */
lam_class_t shmem_pool_t_class = {
    "shmem_pool_t",
    OBJ_CLASS(lam_object_t), 
    (lam_construct_t) lam_mp_shared_construct,
    (lam_destruct_t) lam_mp_destruct
};

void lam_mp_construct(lam_mem_pool_t *pool)
{
    pool->mp_private_alloc = OBJ_NEW(lam_allocator_t);
    lam_mutex_init(&(pool->mp_lock));
    pool->mp_dev_alloc = NULL;
}

void lam_mp_shared_construct(lam_mem_pool_t *pool)
{
    pool->mp_private_alloc = OBJ_NEW(lam_allocator_t);
    lam_mutex_init(&(pool->mp_lock));
    lam_allocator_set_is_shared(pool->mp_private_alloc, 1);
    lam_allocator_set_mem_prot(pool->mp_private_alloc, MMAP_SHARED_PROT);
    pool->mp_dev_alloc = NULL;    
}

void lam_mp_destruct(lam_mem_pool_t *pool)
{
    if ( pool->mp_dev_alloc )
        OBJ_RELEASE(pool->mp_dev_alloc);
    OBJ_RELEASE(pool->mp_private_alloc);

}

int lam_mp_construct_with(lam_mem_pool_t *pool, uint64_t pool_size,
                  uint64_t max_len,
                  uint64_t chunk_size, size_t page_size)
{
    char        *ptr = 0;
    ssize_t     wrk_size = pool_size;
    void        *base = 0;
    ssize_t     to_alloc;
    int         retval, chunk;
    
    pool->mp_page_sz = page_size;
    if (((pool->mp_page_sz / getpagesize()) * getpagesize()) != pool->mp_page_sz)
    {
        return LAM_ERR_BAD_PARAM;
    }
    
    pool->mp_chunk_sz = chunk_size;
    if ( !chunk_size )
    {
        return LAM_ERR_BAD_PARAM;
    }
    
    /* set upper limit on pool */
    if (max_len < 0)
    {
        /* no upper limit on size */
        pool->mp_max_chunks = -1;
    } 
    else
    {
        pool->mp_max_chunks = ((max_len - 1) / page_size) + 1;
        if (pool->mp_max_chunks == 0)
        {
            return LAM_ERR_BAD_PARAM;
        }
    }
    
    /* round up pool size to multiple of page size */
    pool_size = ((((pool_size - 1) / chunk_size) + 1) * chunk_size);
    if (0 == pool_size) {
      lam_output(0, "Error: pool_size == 0");
      return LAM_ERR_BAD_PARAM;
    }
    
    if (pool_size < chunk_size) {
      lam_output(0, "Error: pool_size < chunk_size");
      return LAM_ERR_BAD_PARAM;
    }
    
    /* add red-zone pages */
    
    /* set up dev allocator to use pinned memory */
    lam_allocator_set_should_pin(pool->mp_dev_alloc, 1);
    lam_allocator_set_pin_offset(pool->mp_dev_alloc, page_size);
    
    while (!ptr && wrk_size) {
        to_alloc = wrk_size + 2 * page_size;
        
        /* allocate memory.  Reset pinned memory size. */
        lam_allocator_set_pin_size(pool->mp_dev_alloc, wrk_size);
        ptr = lam_allocator_alloc(pool->mp_dev_alloc, to_alloc);
        if (ptr == 0)
            wrk_size /= 2;
        else
        {
            base = ptr + page_size;
        }
    }
    
    
    /* reset pool size */
    pool_size = wrk_size;
    pool->mp_num_chunks = ((pool_size - 1) / chunk_size) + 1;
    if ((pool->mp_num_chunks > pool->mp_max_chunks) && (pool->mp_max_chunks > 0))
    {
      lam_output(0, "Error: NPoolChunks (%ld) > maxNPoolChunks (%ld)",
                 pool->mp_num_chunks, pool->mp_max_chunks);
      return LAM_ERR_BAD_PARAM;
    }
    /* change memory protection for red zones */
    retval = mprotect(ptr, page_size, PROT_NONE);
    if (retval != 0)
    {
      lam_abort(1, "Error in red zone 1 mprotect");
    }
    /* end red zone */
    retval =
        mprotect(ptr + page_size + wrk_size, page_size, PROT_NONE);
    if (retval != 0)
    {
      lam_abort(1, "Error in red zone 2 mprotect");
    }
    
    /* initialize chunk descriptors */
    to_alloc = sizeof(lam_chunk_desc_t) * pool->mp_num_chunks;
    pool->mp_chunks = lam_allocator_alloc(pool->mp_private_alloc, to_alloc);
    if ( !pool->mp_chunks )
    {
      lam_output(0, "Error: Out of memory");
      return LAM_ERROR;
    }
    
    ptr = (char *) base;
    for ( chunk = 0; chunk < pool->mp_num_chunks; chunk++ )
    {
        pool->mp_chunks[chunk].chd_flags = ALLOCELEMENT_FLAG_AVAILABLE;
        pool->mp_chunks[chunk].chd_index = -1;
        pool->mp_chunks[chunk].chd_base_ptr = ptr;
        ptr += chunk_size;
    }
    /* set next available chunk */
    pool->mp_next_avail_chunk = 0;
    
    return 1;
}


void *lam_mp_request_chunk(lam_mem_pool_t *pool, int pool_index)
{
    void        *chunk = 0;
    int         chunk_found;
    int         next_chunk_to_use;
    lam_chunk_desc_t    *chunk_desc;
    size_t      to_alloc;
    int         desc;
    
    /* grab lock on pool */
    lam_mutex_lock(&(pool->mp_lock));
    
    /* Have we used all the allocated memory? */
    if ( pool->mp_next_avail_chunk == pool->mp_num_chunks )
    {
        
        /* can we increase the pool size ?  We currently won't grow a shared
          memory region. */
        if ( lam_mp_uses_shared_mem(pool)  ||
            ((pool->mp_max_chunks > 0) && (pool->mp_num_chunks == pool->mp_max_chunks)) ) 
        {
            lam_mutex_unlock(&(pool->mp_lock));
            return chunk;
        }
        
        /* allocate larger array of chunk descriptors and
            copy old array into new array */
        to_alloc = sizeof(lam_chunk_desc_t) * (pool->mp_num_chunks + 1);
        chunk_desc = lam_allocator_alloc(pool->mp_private_alloc, to_alloc);
        if ( !chunk_desc )
        {
            lam_output(0, "Error! Out of memory!");
            lam_mutex_unlock(&(pool->mp_lock));
            return 0;
        }
        
        for ( desc = 0; desc < pool->mp_num_chunks; desc++ ) {
            chunk_desc[desc] = pool->mp_chunks[desc];
        }
        
        /* free old array and set old array pointer to point to new array */
        lam_allocator_free(pool->mp_private_alloc, pool->mp_chunks);
        pool->mp_chunks = chunk_desc;
        
        /* allocate new memory chunk using device allocator. */
        lam_allocator_set_should_pin(pool->mp_dev_alloc, 1);
        lam_allocator_set_pin_offset(pool->mp_dev_alloc, 0);
        lam_allocator_set_pin_size(pool->mp_dev_alloc, 0);
        
        pool->mp_chunks[pool->mp_num_chunks].chd_base_ptr =
            lam_allocator_alloc(pool->mp_dev_alloc, pool->mp_chunk_sz);
        if ( !pool->mp_chunks[pool->mp_num_chunks].chd_base_ptr )
        {
          lam_output(0, "Error: Out of memory");
          lam_mutex_unlock(&(pool->mp_lock));
          return chunk;
        }
        
        /* reset pool chunk counter */
        pool->mp_num_chunks++;
    }
    
    /* grab chunk */
    chunk = pool->mp_chunks[pool->mp_next_avail_chunk].chd_base_ptr;
    pool->mp_chunks[pool->mp_next_avail_chunk].chd_flags = ALLOCELEMENT_FLAG_INUSE;
    pool->mp_chunks[pool->mp_next_avail_chunk].chd_index = pool_index;
    
    /* find next available chunk */
    chunk_found = 0;
    next_chunk_to_use = pool->mp_next_avail_chunk + 1;
    while ( next_chunk_to_use < pool->mp_num_chunks )
    {
        if ( pool->mp_chunks[next_chunk_to_use].chd_flags ==
            ALLOCELEMENT_FLAG_AVAILABLE )
        {
            pool->mp_next_avail_chunk = next_chunk_to_use;
            chunk_found = 1;
            break;
        }
        
        next_chunk_to_use++;
    }
    
    /* if no chunks available set next chunk past end of list so that next
       time around more memory will be allocated */
    if ( !chunk_found ) {
        pool->mp_next_avail_chunk = pool->mp_num_chunks;
    }
    lam_mutex_unlock(&(pool->mp_lock));
    return chunk;
}


/*
 *
 *      Fixed shared mem pool interface
 *
 */


lam_class_t lam_fixed_mpool_t_class = {
    "lam_fixed_mpool_t",
    OBJ_CLASS(lam_object_t), 
    (lam_construct_t) lam_fmp_construct,
    (lam_destruct_t) lam_fmp_destruct
};

void lam_fmp_construct(lam_fixed_mpool_t *pool)
{
    pool->fmp_private_alloc = OBJ_NEW(lam_allocator_t);
    lam_allocator_set_is_shared(pool->fmp_private_alloc, 1);
    lam_allocator_set_mem_prot(pool->fmp_private_alloc, MMAP_SHARED_PROT);
    
    pool->fmp_segments = NULL;
    pool->fmp_n_segments = NULL;
    pool->fmp_n_segs_in_array = NULL;
    pool->fmp_min_alloc_size = 0;
    pool->fmp_n_elts_to_add = 0;
    pool->fmp_n_pools = 0;
    pool->fmp_pool_ok_to_use = 0;
    pool->fmp_apply_affinity = 0;
}

void lam_fmp_destruct(lam_fixed_mpool_t *pool)
{
    int         i;
    
    if ( pool->fmp_segments )
    {
        for ( i = 0; i < pool->fmp_n_pools; i++ )
            OBJ_RELEASE(pool->fmp_segments[i]);
        free(pool->fmp_segments);
    }
    
    if ( pool->fmp_n_segments )
        free(pool->fmp_n_segments);
    
    if ( pool->fmp_n_segs_in_array )
        free(pool->fmp_n_segs_in_array);

}



int lam_fmp_construct_with(lam_fixed_mpool_t *pool, ssize_t initial_allocation, 
                       ssize_t min_allocation_size,
                   int n_pools, int n_array_elements_to_add, int apply_mem_affinity)
{
    int     pool_idx;
    void    *ptr;
    
    pool->fmp_pool_ok_to_use = 1;
    pool->fmp_apply_affinity = apply_mem_affinity;
    pool->fmp_min_alloc_size = min_allocation_size;
    if (pool->fmp_min_alloc_size < (ssize_t)getpagesize())
        pool->fmp_min_alloc_size = getpagesize();
    pool->fmp_n_elts_to_add = n_array_elements_to_add;
    pool->fmp_n_pools = n_pools;
    pool->fmp_segments = (lam_memseg_t **) 
      malloc(sizeof(lam_memseg_t *)*n_pools);
    if ( !pool->fmp_segments )
    {
      lam_abort(1, "Unable to allocate memory for "
                "pool->fmp_segments, requested %ld bytes, errno %d",
                sizeof(int) * n_pools, errno);
    }
    memset(pool->fmp_segments, 0, sizeof(lam_memseg_t *)*n_pools);

    pool->fmp_n_segs_in_array = malloc(sizeof(int) * n_pools);
    if ( !pool->fmp_n_segs_in_array ) {
      lam_abort(1, "Unable to allocate memory for "
                "pool->fmp_n_segs_in_array, requested %ld bytes, errno %d",
                sizeof(int) * n_pools, errno);
    }
    bzero(pool->fmp_n_segs_in_array, sizeof(int) * n_pools);
    
    for ( pool_idx = 0; pool_idx < n_pools; pool_idx++ )
    {
        
        ptr = lam_zero_alloc(initial_allocation, MMAP_SHARED_PROT,
                                 MMAP_SHARED_FLAGS);
        if ( !ptr ) {
          lam_abort(1, "Unable to allocate "
                    "memory pool , requested %ld, errno %d",
                    initial_allocation, errno);
        }

        if ( apply_mem_affinity ) 
        {
            if (!lam_set_affinity(ptr, initial_allocation, pool_idx))
            {
              lam_abort(1, "Error: setting memory affinity");
            }
        }
        
        /* set lam_memseg_t data */
        pool->fmp_segments[pool_idx][0].ms_base_ptr = ptr;
        pool->fmp_segments[pool_idx][0].ms_current_ptr = ptr;
        pool->fmp_segments[pool_idx][0].ms_length = initial_allocation;
        pool->fmp_segments[pool_idx][0].ms_mem_available = initial_allocation;
        
        /* update the number of elements in use */
        pool->fmp_n_segments[pool_idx] = 1;
        
    }                       /* end pool loop */
 
    return LAM_SUCCESS;
}

void *lam_fmp_get_mem_segment(lam_fixed_mpool_t *pool,
                              size_t length, size_t alignment, int which_pool)
{
    void    *segment = NULL;
    size_t  mask;
    int     idx, seg_idx;
    ssize_t len_to_alloc;
    char    *ptr;
    lam_memseg_t *tmp_seg;
    
    /* return if pool can't be used */
    if ( !pool->fmp_pool_ok_to_use )
        return NULL;
    
    /* get the appropriate mask for the alignment */
    mask = ~(alignment - 1);
    
    /* loop over pool->fmp_segments elements to see if available memory
        exists */
    idx = -1;
    len_to_alloc = length;
    for ( seg_idx = 0; seg_idx < pool->fmp_n_segments[which_pool];
         seg_idx++ )
    {
        
        ptr =
        (char *) pool->fmp_segments[which_pool][seg_idx].ms_current_ptr;
        
        /* check to see if pointer is aligned correctly */
        if ( (((size_t) ptr) & mask) == ((size_t) ptr) )
        {
            segment = ptr;
            len_to_alloc = length;
        }
        else
        {
            /* align the pointer */
            ptr = (char *) ((size_t) ptr + alignment);
            ptr = (char *) ((size_t) ptr & mask);
            
            len_to_alloc = length +
                (ptr - (char *) pool->fmp_segments[which_pool][seg_idx].ms_current_ptr);
            
            /* continue if not enough memory in this segment */
            if (len_to_alloc >
                pool->fmp_segments[which_pool][seg_idx].ms_mem_available) {
                continue;
            }
            segment = ptr;
        }
        
        if (pool->fmp_segments[which_pool][seg_idx].ms_mem_available >=
            len_to_alloc)
        {
            idx = seg_idx;
            break;
        }
    }
    
    /* if no available memory exists - get more memory */
    if ( idx < 0 )
    {
        /* if need be, increase the size of pool->fmp_segments[] */
        if (pool->fmp_n_segments[which_pool] ==
            pool->fmp_n_segs_in_array[which_pool])
        {
            /* create a temp version of pool->fmp_segments[] */
            tmp_seg = malloc(sizeof(lam_memseg_t) *
                             (pool->fmp_n_segments[which_pool] +
                              pool->fmp_n_elts_to_add));
            if ( !tmp_seg ) {
              lam_abort(1, "Unable to allocate memory for tmp_seg, errno %d",
                        errno);
            }
            /* copy old version of pool->fmp_segments to tmp copy */
            for (seg_idx = 0; seg_idx < pool->fmp_n_segments[which_pool]; seg_idx++)
            {
                tmp_seg[seg_idx].ms_base_ptr =
                pool->fmp_segments[which_pool][seg_idx].ms_base_ptr;
                tmp_seg[seg_idx].ms_current_ptr =
                    pool->fmp_segments[which_pool][seg_idx].ms_current_ptr;
                tmp_seg[seg_idx].ms_length =
                    pool->fmp_segments[which_pool][seg_idx].ms_length;
                tmp_seg[seg_idx].ms_mem_available =
                    pool->fmp_segments[which_pool][seg_idx].ms_mem_available;
            }
            
            free(pool->fmp_segments[which_pool]);
            pool->fmp_segments[which_pool] = tmp_seg;
            
            /* set the element of pool->fmp_segments to grab */
            pool->fmp_n_segs_in_array[which_pool] += pool->fmp_n_elts_to_add;
            
        }                       /* end increase size of pool->fmp_segments[] */
        
        idx = pool->fmp_n_segments[which_pool];
        
        /* allocate more memory */
        len_to_alloc = 4 * (length + alignment);
        if (len_to_alloc < pool->fmp_min_alloc_size)
            len_to_alloc = 2 * pool->fmp_min_alloc_size;
        void *tmp_ptr =
            lam_zero_alloc(len_to_alloc, MMAP_SHARED_PROT, MMAP_SHARED_FLAGS);
        if ( !tmp_ptr )
        {
          lam_abort(1, "Unable to allocate memory pool");
        }
        
        if ( pool->fmp_apply_affinity )
        {
          if ( !lam_set_affinity(tmp_ptr, len_to_alloc, which_pool) ) {
            lam_abort(1, "Error: setting memory affinity");
          }
        }
        
        /* fill in pool->fmp_segments */
        pool->fmp_segments[which_pool][idx].ms_base_ptr = tmp_ptr;
        pool->fmp_segments[which_pool][idx].ms_current_ptr = tmp_ptr;
        pool->fmp_segments[which_pool][idx].ms_length =
            len_to_alloc;
        pool->fmp_segments[which_pool][idx].ms_mem_available =
            len_to_alloc;
        
        pool->fmp_n_segments[which_pool]++;
        
        /* set pointer and length */
        ptr =
            (char *) pool->fmp_segments[which_pool][idx].ms_current_ptr;
        /* check to see if pointer is aligned correctly */
        if ((((size_t) ptr) & mask) == ((size_t) ptr)) {
            segment = ptr;
            len_to_alloc = length;
        } else {
            
            /* align the pointer */
            ptr = (char *) ((size_t) ptr + alignment);
            ptr = (char *) ((size_t) ptr & mask);
            
            segment = ptr;
            len_to_alloc = length +
                (ptr -
                 (char *) pool->fmp_segments[which_pool][idx].
                 ms_current_ptr);
        }
        
    }                           /* end  " idx < 0 " */
    
    /* update pool->fmp_segments */
    pool->fmp_segments[which_pool][idx].ms_current_ptr = (void *)
        ((char *) (pool->fmp_segments[which_pool][idx].ms_current_ptr) +
         len_to_alloc);
    
    pool->fmp_segments[which_pool][idx].ms_mem_available -=
        len_to_alloc;
    
    return segment;
}
