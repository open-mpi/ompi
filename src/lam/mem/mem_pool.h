/*
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
                                                                                                                     
#ifndef _MEMORY_POOL_
#define _MEMORY_POOL_

#include "lam_types.h"
#include "lam/base/object.h"
#include "lam/mem/allocator.h"
#include "lam/threads/mutex.h"

#define ALLOCELEMENT_FLAG_UNUSABLE  (0)
#define ALLOCELEMENT_FLAG_AVAILABLE (1)
#define ALLOCELEMENT_FLAG_INUSE     (2)
#define ALLOCELEMENT_FLAG_NEVERFREE (4)
#define ALLOCELEMENT_FLAG_LOANED    (8)
                                                                                                                      
/*
    To create a process-private pool, use
 CREATE_OBJECT(pool, lam_mem_pool_t, &mem_pool_cls);
 
    To create a process-shared pool, use
 CREATE_OBJECT(pool, lam_mem_pool_t, &shmem_pool_cls);
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
extern lam_class_info_t mem_pool_cls;

/* process-shared mem pool class */
extern lam_class_info_t shmem_pool_cls;

void lam_mp_init(lam_mem_pool_t *pool);
void lam_mp_shared_init(lam_mem_pool_t *pool);
void lam_mp_destroy(lam_mem_pool_t *pool);

int lam_mp_init_with(lam_mem_pool_t *pool, uint64_t pool_size,
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
    lam_alc_get_is_shared(pool->mp_private_alloc)

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

extern lam_class_info_t fixed_mem_pool_cls;

void lam_fmp_init(lam_fixed_mpool_t *pool);
void lam_fmp_destroy(lam_fixed_mpool_t *pool);
int lam_fmp_init_with(lam_fixed_mpool_t *pool, ssize_t initial_allocation, 
                       ssize_t min_allocation_size,
                       int n_pools, int n_array_elements_to_add, int apply_mem_affinity);
void *lam_fmp_get_mem_segment(lam_fixed_mpool_t *pool,
                              size_t length, size_t align, int which_pool);

#endif

