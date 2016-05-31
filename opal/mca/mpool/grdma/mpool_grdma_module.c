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
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
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

static inline bool registration_is_cacheable(mca_mpool_base_registration_t *reg)
{
    return (mca_mpool_grdma_component.leave_pinned &&
            !(reg->flags &
              (MCA_MPOOL_FLAGS_CACHE_BYPASS |
               MCA_MPOOL_FLAGS_PERSIST |
               MCA_MPOOL_FLAGS_INVALID)));
}

#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory(mca_mpool_base_module_t *mpool, void *addr, size_t size);
#endif /* OPAL_CUDA_GDR_SUPPORT */
static void mca_mpool_grdma_pool_contructor (mca_mpool_grdma_pool_t *pool)
{
    memset ((void *)((uintptr_t)pool + sizeof (pool->super)), 0, sizeof (*pool) - sizeof (pool->super));

    OBJ_CONSTRUCT(&pool->lru_list, opal_list_t);
    OBJ_CONSTRUCT(&pool->gc_lifo, opal_lifo_t);

    pool->rcache = mca_rcache_base_module_create(mca_mpool_grdma_component.rcache_name);
}

static void mca_mpool_grdma_pool_destructor (mca_mpool_grdma_pool_t *pool)
{
    OBJ_DESTRUCT(&pool->lru_list);
    OBJ_DESTRUCT(&pool->gc_lifo);

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
    mpool->super.mpool_ft_event = NULL;
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

    rc = mpool_grdma->resources.deregister_mem(mpool_grdma->resources.reg_data, reg);
    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_free_list_return_mt (&mpool_grdma->reg_list,
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

    if(OPAL_SUCCESS != mca_mpool_grdma_register(mpool, addr, size, flags,
                                                MCA_MPOOL_ACCESS_ANY, reg)) {
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

    /* Remove registration from garbage collection list before deregistering it */
    while (NULL != (item = opal_lifo_pop_atomic (&mpool_grdma->pool->gc_lifo))) {
        dereg_mem ((mca_mpool_base_registration_t *) item);
    }
}

static inline bool mca_mpool_grdma_evict_lru_local (mca_mpool_grdma_pool_t *pool)
{
    mca_mpool_grdma_module_t *mpool_grdma;
    mca_mpool_base_registration_t *old_reg;

    opal_mutex_lock (&pool->rcache->lock);
    old_reg = (mca_mpool_base_registration_t *)
        opal_list_remove_first (&pool->lru_list);
    opal_mutex_unlock (&pool->rcache->lock);
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

struct mca_rcache_base_find_args_t {
    mca_mpool_base_registration_t *reg;
    mca_mpool_grdma_module_t *mpool_grdma;
    unsigned char *base;
    unsigned char *bound;
    int access_flags;
};

typedef struct mca_rcache_base_find_args_t mca_rcache_base_find_args_t;

static int mca_mpool_grdma_check_cached (mca_mpool_base_registration_t *grdma_reg, void *ctx)
{
    mca_rcache_base_find_args_t *args = (mca_rcache_base_find_args_t *) ctx;
    mca_mpool_grdma_module_t *mpool_grdma = args->mpool_grdma;

    if ((grdma_reg->flags & MCA_MPOOL_FLAGS_INVALID) || &mpool_grdma->super != grdma_reg->mpool ||
        grdma_reg->base > args->base || grdma_reg->bound < args->bound) {
        return 0;
    }

    if (OPAL_UNLIKELY((args->access_flags & grdma_reg->access_flags) != args->access_flags)) {
        args->access_flags |= grdma_reg->access_flags;

        if (0 != grdma_reg->ref_count) {
            if (!(grdma_reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS)) {
                grdma_reg->mpool->rcache->rcache_delete (grdma_reg->mpool->rcache, grdma_reg);
            }

            /* mark the registration to go away when it is deregistered */
            grdma_reg->flags |= MCA_MPOOL_FLAGS_INVALID | MCA_MPOOL_FLAGS_CACHE_BYPASS;
        } else {
            if (registration_is_cacheable(grdma_reg)) {
                opal_list_remove_item (&mpool_grdma->pool->lru_list, (opal_list_item_t *) grdma_reg);
            }

            dereg_mem (grdma_reg);
        }
    } else {
        if (0 == grdma_reg->ref_count) {
            /* Leave pinned must be set for this to still be in the rcache. */
            opal_list_remove_item(&mpool_grdma->pool->lru_list,
                                  (opal_list_item_t *) grdma_reg);
        }

        /* This segment fits fully within an existing segment. */
        mpool_grdma->stat_cache_hit++;
        (void) opal_atomic_add_32 (&grdma_reg->ref_count, 1);
        args->reg = grdma_reg;
        return 1;
    }

    /* can't use this registration */
    return 0;
}

/*
 * register memory
 */
int mca_mpool_grdma_register (mca_mpool_base_module_t *mpool, void *addr,
                              size_t size, uint32_t flags, int32_t access_flags,
                              mca_mpool_base_registration_t **reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    const bool bypass_cache = !!(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS);
    const bool persist = !!(flags & MCA_MPOOL_FLAGS_PERSIST);
    mca_mpool_base_registration_t *grdma_reg;
    opal_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    *reg = NULL;

    /* if cache bypass is requested don't use the cache */
    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
                                            mca_mpool_base_page_size_log);

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

    do_unregistration_gc(mpool);

    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(bypass_cache || persist)) {
        mca_rcache_base_find_args_t find_args = {.reg = NULL, .mpool_grdma = mpool_grdma,
                                                 .base = base, .bound = bound,
                                                 .access_flags = access_flags};

        /* check to see if memory is registered */
        rc = mpool->rcache->rcache_iterate (mpool->rcache, base, size,
                                            mca_mpool_grdma_check_cached, (void *) &find_args);
        if (1 == rc) {
            *reg = find_args.reg;
            return OPAL_SUCCESS;
        }

        /* get updated access flags */
        access_flags = find_args.access_flags;

        OPAL_THREAD_ADD32((volatile int32_t *) &mpool_grdma->stat_cache_miss, 1);
    }

    item = opal_free_list_get_mt (&mpool_grdma->reg_list);
    if(NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    grdma_reg = (mca_mpool_base_registration_t*)item;

    grdma_reg->mpool = mpool;
    grdma_reg->base = base;
    grdma_reg->bound = bound;
    grdma_reg->flags = flags;
    grdma_reg->access_flags = access_flags;
    grdma_reg->ref_count = 1;
#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_MPOOL_FLAGS_CUDA_GPU_MEM) {
        mca_common_cuda_get_buffer_id(grdma_reg);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    while (OPAL_ERR_OUT_OF_RESOURCE ==
           (rc = mpool_grdma->resources.register_mem(mpool_grdma->resources.reg_data,
                                                     base, bound - base + 1, grdma_reg))) {
        /* try to remove one unused reg and retry */
        if (!mca_mpool_grdma_evict (mpool)) {
            break;
        }
    }

    if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
        opal_free_list_return_mt (&mpool_grdma->reg_list, item);
        return rc;
    }

    if (false == bypass_cache) {
        rc = mpool->rcache->rcache_insert(mpool->rcache, grdma_reg, 0);

        if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
            opal_free_list_return_mt (&mpool_grdma->reg_list, item);
            return rc;
        }
    }

    *reg = grdma_reg;

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

    opal_mutex_lock (&mpool->rcache->lock);

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
        (void) opal_atomic_add_32 (&(*reg)->ref_count, 1);
    } else {
        mpool_grdma->stat_cache_notfound++;
    }

    opal_mutex_unlock (&mpool->rcache->lock);

    return rc;
}

int mca_mpool_grdma_deregister(struct mca_mpool_base_module_t *mpool,
                               mca_mpool_base_registration_t *reg)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) mpool;
    int32_t ref_count;
    int rc;

    opal_mutex_lock (&mpool_grdma->pool->rcache->lock);
    ref_count = opal_atomic_add_32 (&reg->ref_count, -1);

    assert (ref_count >= 0);
    if (ref_count > 0) {
        opal_mutex_unlock (&mpool_grdma->pool->rcache->lock);

        return OPAL_SUCCESS;
    }

    if (registration_is_cacheable(reg)) {
        opal_list_append(&mpool_grdma->pool->lru_list, (opal_list_item_t *) reg);
        opal_mutex_unlock (&mpool_grdma->pool->rcache->lock);

        return OPAL_SUCCESS;
    }

    rc = dereg_mem (reg);
    opal_mutex_unlock (&mpool_grdma->pool->rcache->lock);

    return rc;
}

static int gc_add (mca_mpool_base_registration_t *grdma_reg, void *ctx)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) grdma_reg->mpool;

    /* unused */
    (void) ctx;

    if (grdma_reg->flags & MCA_MPOOL_FLAGS_INVALID) {
        /* nothing more to do */
        return OPAL_SUCCESS;
    }

    if (grdma_reg->ref_count) {
        /* attempted to remove an active registration */
        return OPAL_ERROR;
    }

    /* This may be called from free() so avoid recursively calling into free by just
     * shifting this registration into the garbage collection list. The cleanup will
     * be done on the next registration attempt. */
    if (registration_is_cacheable (grdma_reg)) {
        opal_list_remove_item (&mpool_grdma->pool->lru_list, (opal_list_item_t *) grdma_reg);
    }

    grdma_reg->flags |= MCA_MPOOL_FLAGS_INVALID;

    opal_lifo_push_atomic (&mpool_grdma->pool->gc_lifo, (opal_list_item_t *) grdma_reg);

    return OPAL_SUCCESS;
}

int mca_mpool_grdma_release_memory(struct mca_mpool_base_module_t *mpool,
                                   void *base, size_t size)
{
    return mpool->rcache->rcache_iterate (mpool->rcache, base, size, gc_add, NULL);
}

/* Make sure this registration request is not stale.  In other words, ensure
 * that we do not have a cuMemAlloc, cuMemFree, cuMemAlloc state.  If we do
 * kick out the regisrations and deregister.  This function needs to be called
 * with the mpool->rcache->lock held. */
#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory(mca_mpool_base_module_t *mpool, void *addr, size_t size)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) mpool;
    mca_mpool_base_registration_t *reg;

    mpool->rcache->rcache_find(mpool->rcache, addr, size, &reg);
    if (NULL == reg) {
        return OPAL_SUCCESS;
    }

    /* If not previously freed memory, just return 0 */
    if (!(mca_common_cuda_previously_freed_memory(reg))) {
        return OPAL_SUCCESS;
    }

    /* This memory has been freed.  Find all registrations and delete. Ensure they are deregistered
     * now by passing dereg_mem as the delete function. This is safe because the rcache lock is
     * recursive and this is only called from register. */
    return mpool->rcache->rcache_iterate (mpool->rcache, addr, size, gc_add, NULL);
}
#endif /* OPAL_CUDA_GDR_SUPPORT */

static int iterate_dereg_finalize (mca_mpool_base_registration_t *grdma_reg, void *ctx)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t *) ctx;

    if ((mca_mpool_base_module_t *) mpool_grdma != grdma_reg->mpool) {
        return 0;
    }

    if (registration_is_cacheable (grdma_reg)) {
        opal_list_remove_item (&mpool_grdma->pool->lru_list, (opal_list_item_t *) grdma_reg);
    }

    /* set the reference count to 0 otherwise dereg will fail on assert */
    grdma_reg->ref_count = 0;

    return dereg_mem (grdma_reg);
}

void mca_mpool_grdma_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;

    /* Statistic */
    if (true == mca_mpool_grdma_component.print_stats) {
        opal_output(0, "%s grdma: stats "
                "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                mpool_grdma->stat_cache_hit, mpool_grdma->stat_cache_miss,
                mpool_grdma->stat_cache_found, mpool_grdma->stat_cache_notfound,
                mpool_grdma->stat_evicted);
    }


    do_unregistration_gc(mpool);

    (void) mpool->rcache->rcache_iterate (mpool->rcache, NULL, (size_t) -1,
                                          iterate_dereg_finalize, (void *) mpool);

    OBJ_RELEASE(mpool_grdma->pool);
    OBJ_DESTRUCT(&mpool_grdma->reg_list);

    /* this mpool was allocated by grdma_init in mpool_grdma_component.c */
    free(mpool);
}

