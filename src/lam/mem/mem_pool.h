/*
 * $HEADER$
 */

#ifndef LAM_MEMORY_POOL_H
#define LAM_MEMORY_POOL_H

#include "lam/types.h"
#include "lam/lfc/object.h"
#include "lam/mem/allocator.h"
#include "lam/threads/mutex.h"

#define ALLOCELEMENT_FLAG_UNUSABLE  (0)
#define ALLOCELEMENT_FLAG_AVAILABLE (1)
#define ALLOCELEMENT_FLAG_INUSE     (2)
#define ALLOCELEMENT_FLAG_NEVERFREE (4)
#define ALLOCELEMENT_FLAG_LOANED    (8)

/*
    To create a process-private pool, use
pool = OBJ_NEW(lam_mem_pool_t);
 
    To create a process-shared pool, use
pool = OBJ_NEW(lam_shmem_pool_t);
 */

typedef struct lam_chunk_desc
{
    uint16_t    chd_flags;
    uint32_t    chd_index;
    void        *chd_base_ptr;
} lam_chunk_desc_t;

typedef struct lam_mem_pool
{
    lam_object_t        super;
    lam_allocator_t    *mp_dev_alloc;   /* possibly device-dependent allocator for registering memory */
    lam_allocator_t    *mp_private_alloc;    /* for use by pool only; do not set! */
    lam_mutex_t         mp_lock;
    uint64_t            mp_page_sz;
    uint64_t            mp_chunk_sz;
    uint32_t            mp_num_chunks;
    uint32_t            mp_max_chunks;
    uint32_t            mp_next_avail_chunk;
    lam_chunk_desc_t   *mp_chunks;
} lam_mem_pool_t;

/* process-private mem pool class */
extern lam_class_info_t lam_mem_pool_t_class_info;

/* process-shared mem pool class */
extern lam_class_info_t shmem_pool_t_class_info;

void lam_mp_construct(lam_mem_pool_t *pool);
void lam_mp_shared_construct(lam_mem_pool_t *pool);
void lam_mp_destruct(lam_mem_pool_t *pool);

int lam_mp_construct_with(lam_mem_pool_t *pool, uint64_t pool_size,
                  uint64_t max_len,
                  uint64_t chunk_size, size_t pg_size);

void *lam_mp_request_chunk(lam_mem_pool_t *pool, int pool_index);

/*
 *
 *      Memory Pool accessor functions
 *
 */

/* returns 1 if pool uses shared memory, 0 otherwise. */
#define lam_mp_uses_shared_mem(pool) \
    lam_allocator_get_is_shared(pool->mp_private_alloc)

#define lam_mp_get_dev_allocator(pool) \
    ((pool)->mp_dev_alloc)

static inline void lam_mp_set_dev_allocator(lam_mem_pool_t *pool, lam_allocator_t *allocator)
{
    /* releases old allocator and retains new one. */
    if ( pool->mp_dev_alloc )
        OBJ_RELEASE(pool->mp_dev_alloc);
    pool->mp_dev_alloc = allocator;
    OBJ_RETAIN(pool->mp_dev_alloc);
}

#define lam_mp_get_chunk_size(pool) \
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

typedef struct lam_mem_segment
{
    void       *ms_base_ptr;
    void       *ms_current_ptr;
    size_t      ms_length;
    size_t      ms_mem_available;
} lam_memseg_t;


typedef struct lam_fixed_mpool
{
    lam_object_t        super;
    lam_allocator_t    *fmp_private_alloc;
    lam_memseg_t      **fmp_segments;
    int                *fmp_n_segments;
    int                *fmp_n_segs_in_array;
    size_t              fmp_min_alloc_size;
    int                 fmp_n_elts_to_add;
    int                 fmp_n_pools;
    int                 fmp_pool_ok_to_use;
    int                 fmp_apply_affinity;
} lam_fixed_mpool_t;

extern lam_class_info_t lam_fixed_mpool_t_class_info;

void lam_fmp_construct(lam_fixed_mpool_t *pool);
void lam_fmp_destruct(lam_fixed_mpool_t *pool);
int  lam_fmp_construct_with(lam_fixed_mpool_t *pool, ssize_t initial_allocation, 
                       ssize_t min_allocation_size,
                       int n_pools, int n_array_elements_to_add, int apply_mem_affinity);
void *lam_fmp_get_mem_segment(lam_fixed_mpool_t *pool,
                              size_t length, size_t align, int which_pool);

#endif /* LAM_MEMORY_POOL_H */
