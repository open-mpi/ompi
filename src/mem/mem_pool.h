/*
 * $HEADER$
 */

#ifndef OMPI_MEMORY_POOL_H
#define OMPI_MEMORY_POOL_H

#include "include/types.h"
#include "class/ompi_object.h"
#include "mem/allocator.h"
#include "threads/mutex.h"

#define ALLOCELEMENT_FLAG_UNUSABLE  (0)
#define ALLOCELEMENT_FLAG_AVAILABLE (1)
#define ALLOCELEMENT_FLAG_INUSE     (2)
#define ALLOCELEMENT_FLAG_NEVERFREE (4)
#define ALLOCELEMENT_FLAG_LOANED    (8)

/*
    To create a process-private pool, use
pool = OBJ_NEW(ompi_mem_pool_t);
 
    To create a process-shared pool, use
pool = OBJ_NEW(ompi_shmem_pool_t);
 */

typedef struct ompi_chunk_desc
{
    uint16_t    chd_flags;
    uint32_t    chd_index;
    void        *chd_base_ptr;
} ompi_chunk_desc_t;

typedef struct ompi_mem_pool
{
    ompi_object_t        super;
    ompi_allocator_t    *mp_dev_alloc;   /* possibly device-dependent allocator for registering memory */
    ompi_allocator_t    *mp_private_alloc;    /* for use by pool only; do not set! */
    ompi_mutex_t         mp_lock;
    uint64_t            mp_page_sz;
    uint64_t            mp_chunk_sz;
    uint32_t            mp_num_chunks;
    uint32_t            mp_max_chunks;
    uint32_t            mp_next_avail_chunk;
    ompi_chunk_desc_t   *mp_chunks;
} ompi_mem_pool_t;

/* process-private mem pool class */
extern ompi_class_t ompi_mem_pool_t_class;

/* process-shared mem pool class */
extern ompi_class_t shmem_pool_t_class;

void ompi_mp_construct(ompi_mem_pool_t *pool);
void ompi_mp_shared_construct(ompi_mem_pool_t *pool);
void ompi_mp_destruct(ompi_mem_pool_t *pool);

int ompi_mp_construct_with(ompi_mem_pool_t *pool, uint64_t pool_size,
                  uint64_t max_len,
                  uint64_t chunk_size, size_t pg_size);

void *ompi_mp_request_chunk(ompi_mem_pool_t *pool, int pool_index);

/*
 *
 *      Memory Pool accessor functions
 *
 */

/* returns 1 if pool uses shared memory, 0 otherwise. */
#define ompi_mp_uses_shared_mem(pool) \
    ompi_allocator_get_is_shared(pool->mp_private_alloc)

#define ompi_mp_get_dev_allocator(pool) \
    ((pool)->mp_dev_alloc)

static inline void ompi_mp_set_dev_allocator(ompi_mem_pool_t *pool, ompi_allocator_t *allocator)
{
    /* releases old allocator and retains new one. */
    if ( pool->mp_dev_alloc )
        OBJ_RELEASE(pool->mp_dev_alloc);
    pool->mp_dev_alloc = allocator;
    OBJ_RETAIN(pool->mp_dev_alloc);
}

#define ompi_mp_get_chunk_size(pool) \
    ((pool)->mp_chunk_sz)

/*
 *
 *      Fixed shared mem pool interface
 *
 */

/*
    Class used to satisfy shared memory requests. Assumes that request
    are made before the child process are forked, and that this memory
    will not be recycled or freed until the app exits.
 */

typedef struct ompi_mem_segment
{
    void       *ms_base_ptr;
    void       *ms_current_ptr;
    size_t      ms_length;
    size_t      ms_mem_available;
} ompi_memseg_t;


typedef struct ompi_fixed_mpool
{
    ompi_object_t        super;
    ompi_allocator_t    *fmp_private_alloc;
    ompi_memseg_t      **fmp_segments;
    int                *fmp_n_segments;
    int                *fmp_n_segs_in_array;
    size_t              fmp_min_alloc_size;
    int                 fmp_n_elts_to_add;
    int                 fmp_n_pools;
    int                 fmp_pool_ok_to_use;
    int                 fmp_apply_affinity;
} ompi_fixed_mpool_t;

extern ompi_class_t ompi_fixed_mpool_t_class;

void ompi_fmp_construct(ompi_fixed_mpool_t *pool);
void ompi_fmp_destruct(ompi_fixed_mpool_t *pool);
int  ompi_fmp_construct_with(ompi_fixed_mpool_t *pool, ssize_t initial_allocation, 
                       ssize_t min_allocation_size,
                       int n_pools, int n_array_elements_to_add, int apply_mem_affinity);
void *ompi_fmp_get_mem_segment(ompi_fixed_mpool_t *pool,
                              size_t length, size_t align, int which_pool);

#endif /* OMPI_MEMORY_POOL_H */
