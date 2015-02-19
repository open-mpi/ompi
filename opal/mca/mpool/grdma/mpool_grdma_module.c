/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "opal_config.h"

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "opal/align.h"

#include "opal/util/proc.h"
#if OPAL_CUDA_GDR_SUPPORT
#include "opal/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_GDR_SUPPORT */
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/rcache/base/base.h"

#include "opal/mca/mpool/base/base.h"
#include "mpool_grdma.h"

#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory(mca_mpool_base_module_t *mpool, void *addr, size_t size);
#endif /* OPAL_CUDA_GDR_SUPPORT */
static void mca_mpool_grdma_pool_contructor (mca_mpool_grdma_pool_t *pool)
{
    memset ((void *)((uintptr_t)pool + sizeof (pool->super)), 0, sizeof (*pool) - sizeof (pool->super));

    OBJ_CONSTRUCT(&pool->lru_list, opal_list_t);
    OBJ_CONSTRUCT(&pool->gc_list, opal_list_t);

    pool->rcache = mca_rcache_base_module_create(mca_mpool_grdma_component.rcache_name);
}

static void mca_mpool_grdma_pool_destructor (mca_mpool_grdma_pool_t *pool)
{
    OBJ_DESTRUCT(&pool->lru_list);
    OBJ_DESTRUCT(&pool->gc_list);

    free (pool->pool_name);
}

OBJ_CLASS_INSTANCE(mca_mpool_grdma_pool_t, opal_list_item_t,
                   mca_mpool_grdma_pool_contructor,
                   mca_mpool_grdma_pool_destructor);

/*
 *  Initializes the mpool module.
 */
void mca_mpool_grdma_module_init(mca_mpool_grdma_module_t* mpool, mca_mpool_grdma_pool_t *pool)
{
    OBJ_RETAIN(pool);
    mpool->pool = pool;

    mpool->super.mpool_component = &mca_mpool_grdma_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = mca_mpool_grdma_alloc;
    mpool->super.mpool_realloc = mca_mpool_grdma_realloc;
    mpool->super.mpool_free = mca_mpool_grdma_free;
    mpool->super.mpool_register = mca_mpool_grdma_register;
    mpool->super.mpool_find = mca_mpool_grdma_find;
    mpool->super.mpool_deregister = mca_mpool_grdma_deregister;
    mpool->super.mpool_release_memory = mca_mpool_grdma_release_memory;
    mpool->super.mpool_finalize = mca_mpool_grdma_finalize;
    mpool->super.mpool_ft_event = mca_mpool_grdma_ft_event;
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM;
    mpool->super.rcache = pool->rcache;

    mpool->stat_cache_hit = mpool->stat_cache_miss = mpool->stat_evicted = 0;
    mpool->stat_cache_found = mpool->stat_cache_notfound = 0;

    OBJ_CONSTRUCT(&mpool->reg_list, opal_free_list_t);
    opal_free_list_init (&mpool->reg_list, mpool->resources.sizeof_reg,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_mpool_base_registration_t),
                         0, opal_cache_line_size, 0, -1, 32, NULL, 0,
                         NULL, NULL, NULL);
}

static inline int dereg_mem(mca_mpool_base_registration_t *reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) reg->mpool;
    int rc;

    if(!(reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS))
        reg->mpool->rcache->rcache_delete(reg->mpool->rcache, reg);

    /* Drop the rcache lock before deregistring the memory */
    OPAL_THREAD_UNLOCK(&reg->mpool->rcache->lock);
    rc = mpool_grdma->resources.deregister_mem(mpool_grdma->resources.reg_data,
                                               reg);
    OPAL_THREAD_LOCK(&reg->mpool->rcache->lock);

    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_free_list_return (&mpool_grdma->reg_list,
                               (opal_free_list_item_t *) reg);
    }

    return rc;
}

/**
  * allocate function
  */
void* mca_mpool_grdma_alloc(mca_mpool_base_module_t *mpool, size_t size,
        size_t align, uint32_t flags, mca_mpool_base_registration_t **reg)
{
    void *base_addr, *addr;

    if(0 == align)
        align = mca_mpool_base_page_size;

#if OPAL_CUDA_SUPPORT
    /* CUDA cannot handle registering overlapping regions, so make
     * sure each region is page sized and page aligned. */
    align = mca_mpool_base_page_size;
    size = OPAL_ALIGN(size, mca_mpool_base_page_size, size_t);
#endif

#ifdef HAVE_POSIX_MEMALIGN
    if((errno = posix_memalign(&base_addr, align, size)) != 0)
        return NULL;

    addr = base_addr;
#else
    base_addr = malloc(size + align);
    if(NULL == base_addr)
        return NULL;

    addr = (void*)OPAL_ALIGN((uintptr_t)base_addr, align, uintptr_t);
#endif

    if(OPAL_SUCCESS != mca_mpool_grdma_register(mpool, addr, size, flags, reg)) {
        free(base_addr);
        return NULL;
    }
    (*reg)->alloc_base = (unsigned char *) base_addr;

    return addr;
}

/* This function must be called with the rcache lock held */
static inline void do_unregistration_gc(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    opal_list_item_t *item;

    /* Remove registration from garbage collection list
       before deregistering it */
    while (NULL != 
           (item = opal_list_remove_first(&mpool_grdma->pool->gc_list))) {
        dereg_mem((mca_mpool_base_registration_t *) item);
    }
}

static inline bool mca_mpool_grdma_evict_lru_local (mca_mpool_grdma_pool_t *pool)
{
    mca_mpool_grdma_module_t *mpool_grdma;
    mca_mpool_base_registration_t *old_reg;

    old_reg = (mca_mpool_base_registration_t *)
        opal_list_remove_first (&pool->lru_list);
    if (NULL == old_reg) {
        return false;
    }

    mpool_grdma = (mca_mpool_grdma_module_t *) old_reg->mpool;

    (void) dereg_mem (old_reg);

    mpool_grdma->stat_evicted++;

    return true;
}

enum {
    MCA_MPOOL_GRDMA_MSG_EMPTY      = 0,
    MCA_MPOOL_GRDMA_MSG_NEED_DEREG = 1,
    MCA_MPOOL_GRDMA_MSG_BUSY       = 2,
    MCA_MPOOL_GRDMA_MSG_COMPLETE   = 3
};

bool mca_mpool_grdma_evict (struct mca_mpool_base_module_t *mpool)
{
    return mca_mpool_grdma_evict_lru_local (((mca_mpool_grdma_module_t *) mpool)->pool);
}

/*
 * register memory
 */
int mca_mpool_grdma_register(mca_mpool_base_module_t *mpool, void *addr,
                              size_t size, uint32_t flags,
                              mca_mpool_base_registration_t **reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    const bool bypass_cache = !!(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS);
    const bool persist = !!(flags & MCA_MPOOL_FLAGS_PERSIST);
    mca_mpool_base_registration_t *grdma_reg;
    opal_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    /* if cache bypass is requested don't use the cache */
    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
                                            mca_mpool_base_page_size_log);
    if (!opal_list_is_empty (&mpool_grdma->pool->gc_list))
        do_unregistration_gc(mpool);

#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_MPOOL_FLAGS_CUDA_GPU_MEM) {
        size_t psize;
        mca_common_cuda_get_address_range(&base, &psize, addr);
        bound = base + psize - 1;
        /* Check to see if this memory is in the cache and if it has been freed. If so,
         * this call will boot it out of the cache. */
        check_for_cuda_freed_memory(mpool, base, psize);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(bypass_cache || persist)) {
        /* check to see if memory is registered */
        mpool->rcache->rcache_find(mpool->rcache, base, bound - base + 1, reg);
        if (*reg && !(flags & MCA_MPOOL_FLAGS_INVALID)) {
            if (0 == (*reg)->ref_count) {
                /* Leave pinned must be set for this to still be in the rcache. */
                opal_list_remove_item(&mpool_grdma->pool->lru_list,
                                      (opal_list_item_t *)(*reg));
            }

            /* This segment fits fully within an existing segment. */
            mpool_grdma->stat_cache_hit++;
            (*reg)->ref_count++;
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return OPAL_SUCCESS;
        }

        mpool_grdma->stat_cache_miss++;
        *reg = NULL; /* in case previous find found something */

        /* Unless explicitly requested by the caller always store the
         * registration in the rcache. This will speed up the case where
         * no leave pinned protocol is in use but the same segment is in
         * use in multiple simultaneous transactions. We used to set bypass_cache
         * here is !mca_mpool_grdma_component.leave_pinned. */
    }

    item = opal_free_list_get (&mpool_grdma->reg_list);
    if(NULL == item) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    grdma_reg = (mca_mpool_base_registration_t*)item;

    grdma_reg->mpool = mpool;
    grdma_reg->base = base;
    grdma_reg->bound = bound;
    grdma_reg->flags = flags;
#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_MPOOL_FLAGS_CUDA_GPU_MEM) {
        mca_common_cuda_get_buffer_id(grdma_reg);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    if (false == bypass_cache) {
        rc = mpool->rcache->rcache_insert(mpool->rcache, grdma_reg, 0);

        if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            opal_free_list_return (&mpool_grdma->reg_list, item);
            return rc;
        }
    }

    while (OPAL_ERR_OUT_OF_RESOURCE ==
           (rc = mpool_grdma->resources.register_mem(mpool_grdma->resources.reg_data,
                                                     base, bound - base + 1, grdma_reg))) {
        /* try to remove one unused reg and retry */
        if (!mca_mpool_grdma_evict (mpool)) {
            break;
        }
    }

    if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
        if (false == bypass_cache) {
            mpool->rcache->rcache_delete(mpool->rcache, grdma_reg);
        }
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        opal_free_list_return (&mpool_grdma->reg_list, item);
        return rc;
    }

    *reg = grdma_reg;
    (*reg)->ref_count++;
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);
    return OPAL_SUCCESS;
}


/**
  * realloc function
  */
void* mca_mpool_grdma_realloc(mca_mpool_base_module_t *mpool, void *addr,
    size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_base_registration_t *old_reg  = *reg;
    void *new_mem = mca_mpool_grdma_alloc(mpool, size, 0, old_reg->flags, reg);
    memcpy(new_mem, addr, old_reg->bound - old_reg->base + 1);
    mca_mpool_grdma_free(mpool, addr, old_reg);

    return new_mem;
}

/**
  * free function
  */
void mca_mpool_grdma_free(mca_mpool_base_module_t *mpool, void *addr,
                         mca_mpool_base_registration_t *registration)
{
    void *alloc_base = registration->alloc_base;
    mca_mpool_grdma_deregister(mpool, registration);
    free(alloc_base);
}

int mca_mpool_grdma_find(struct mca_mpool_base_module_t *mpool, void *addr,
        size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    unsigned char *base, *bound;
    int rc;

    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
             mca_mpool_base_page_size_log);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    rc = mpool->rcache->rcache_find(mpool->rcache, base, bound - base + 1, reg);
    if(NULL != *reg &&
            (mca_mpool_grdma_component.leave_pinned ||
             ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
             ((*reg)->base == base && (*reg)->bound == bound))) {
        assert(((void*)(*reg)->bound) >= addr);
        if(0 == (*reg)->ref_count &&
                mca_mpool_grdma_component.leave_pinned) {
            opal_list_remove_item(&mpool_grdma->pool->lru_list,
                                  (opal_list_item_t*)(*reg));
        }
        mpool_grdma->stat_cache_found++;
        (*reg)->ref_count++;
    } else {
        mpool_grdma->stat_cache_notfound++;
    }

    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

static inline bool registration_is_cacheable(mca_mpool_base_registration_t *reg)
{
    return (mca_mpool_grdma_component.leave_pinned &&
            !(reg->flags &
              (MCA_MPOOL_FLAGS_CACHE_BYPASS |
               MCA_MPOOL_FLAGS_PERSIST |
               MCA_MPOOL_FLAGS_INVALID)));
}

int mca_mpool_grdma_deregister(struct mca_mpool_base_module_t *mpool,
                               mca_mpool_base_registration_t *reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) mpool;
    int rc = OPAL_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg->ref_count--;
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OPAL_SUCCESS;
    }

    if(registration_is_cacheable(reg)) {
        opal_list_append(&mpool_grdma->pool->lru_list, (opal_list_item_t *) reg);
    } else {
        rc = dereg_mem (reg);
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

    return rc;
}

#define GRDMA_MPOOL_NREGS 100

int mca_mpool_grdma_release_memory(struct mca_mpool_base_module_t *mpool,
                                   void *base, size_t size)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) mpool;
    mca_mpool_base_registration_t *regs[GRDMA_MPOOL_NREGS];
    int reg_cnt, i, rc = OPAL_SUCCESS;

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, base, size,
                                                 regs, GRDMA_MPOOL_NREGS);

        for(i = 0 ; i < reg_cnt ; ++i) {
            regs[i]->flags |= MCA_MPOOL_FLAGS_INVALID;
            if (regs[i]->ref_count) {
                /* memory is being freed, but there are registration in use that
                 * covers the memory. This can happen even in a correct program,
                 * but may also be an user error. We can't tell. Mark the
                 * registration as invalid. It will not be used any more and
                 * will be unregistered when ref_count will become zero */
                rc = OPAL_ERROR; /* tell caller that something was wrong */
            } else {
                opal_list_remove_item(&mpool_grdma->pool->lru_list,(opal_list_item_t *) regs[i]);
                opal_list_append(&mpool_grdma->pool->gc_list, (opal_list_item_t *) regs[i]);
            }
        }
    } while(reg_cnt == GRDMA_MPOOL_NREGS);

    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

/* Make sure this registration request is not stale.  In other words, ensure
 * that we do not have a cuMemAlloc, cuMemFree, cuMemAlloc state.  If we do
 * kick out the regisrations and deregister.  This function needs to be called
 * with the mpool->rcache->lock held. */
#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory(mca_mpool_base_module_t *mpool, void *addr, size_t size)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) mpool;
    mca_mpool_base_registration_t *regs[GRDMA_MPOOL_NREGS];
    int reg_cnt, i, rc = OPAL_SUCCESS;
    mca_mpool_base_registration_t *reg;

    mpool->rcache->rcache_find(mpool->rcache, addr, size, &reg);
    if (NULL == reg) {
        return OPAL_SUCCESS;
    }

    /* If not previously freed memory, just return 0 */
    if (!(mca_common_cuda_previously_freed_memory(reg))) {
        return OPAL_SUCCESS;
    }

    /* mpool->rcache->rcache_dump_range(mpool->rcache, 0, (size_t)-1, "Before free"); */

    /* This memory has been freed.  Find all registrations and delete */
    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, reg->base, reg->bound - reg->base + 1,
                                                 regs, GRDMA_MPOOL_NREGS);
        for(i = 0 ; i < reg_cnt ; ++i) {
            regs[i]->flags |= MCA_MPOOL_FLAGS_INVALID;
            if (regs[i]->ref_count) {
                opal_output(0, "Release FAILED: ref_count=%d, base=%p, bound=%p, size=%d",
                            regs[i]->ref_count, regs[i]->base, regs[i]->bound,
                            (int) (regs[i]->bound - regs[i]->base + 1));
                /* memory is being freed, but there are registration in use that
                 * covers the memory. This can happen even in a correct program,
                 * but may also be an user error. We can't tell. Mark the
                 * registration as invalid. It will not be used any more and
                 * will be unregistered when ref_count will become zero */
                rc = OPAL_ERROR; /* tell caller that something was wrong */
            } else {
                opal_list_remove_item(&mpool_grdma->pool->lru_list,(opal_list_item_t *) regs[i]);
                /* Now deregister.  Do not use gc_list as we need to kick this out now. */
                dereg_mem(regs[i]);
            }
        }
    } while(reg_cnt == GRDMA_MPOOL_NREGS);

    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
    /* mpool->rcache->rcache_dump_range(mpool->rcache, 0, (size_t)-1, "After free");*/

    return rc;
}
#endif /* OPAL_CUDA_GDR_SUPPORT */

void mca_mpool_grdma_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    mca_mpool_base_registration_t *regs[GRDMA_MPOOL_NREGS];
    int reg_cnt, i;

    /* Statistic */
    if (true == mca_mpool_grdma_component.print_stats) {
        opal_output(0, "%s grdma: stats "
                "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                mpool_grdma->stat_cache_hit, mpool_grdma->stat_cache_miss,
                mpool_grdma->stat_cache_found, mpool_grdma->stat_cache_notfound,
                mpool_grdma->stat_evicted);
    }

    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    do_unregistration_gc(mpool);

    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, 0, (size_t)-1,
                                                 regs, GRDMA_MPOOL_NREGS);

        for (i = 0 ; i < reg_cnt ; ++i) {
            if (regs[i]->ref_count) {
                regs[i]->ref_count = 0; /* otherwise dereg will fail on assert */
            } else if (mca_mpool_grdma_component.leave_pinned) {
                opal_list_remove_item(&mpool_grdma->pool->lru_list,
                                      (opal_list_item_t *) regs[i]);
            }

	    (void) dereg_mem(regs[i]);
        }
    } while (reg_cnt == GRDMA_MPOOL_NREGS);

    OBJ_RELEASE(mpool_grdma->pool);

    OBJ_DESTRUCT(&mpool_grdma->reg_list);
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

    /* this mpool was allocated by grdma_init in mpool_grdma_component.c */
    free(mpool);
}

int mca_mpool_grdma_ft_event(int state) {
    return OPAL_SUCCESS;
}
