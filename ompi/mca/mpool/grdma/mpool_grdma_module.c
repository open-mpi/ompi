/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"

#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "opal/align.h"

#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/runtime/params.h"

#include "ompi/mca/mpool/base/base.h"
#include "mpool_grdma.h"

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

    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t);
    ompi_free_list_init_new(&mpool->reg_list, mpool->resources.sizeof_reg,
                            opal_cache_line_size,
                            OBJ_CLASS(mca_mpool_base_registration_t), 
                            0, opal_cache_line_size, 0, -1, 32, NULL);
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

    if (OPAL_LIKELY(OMPI_SUCCESS == rc)) {
        OMPI_FREE_LIST_RETURN(&mpool_grdma->reg_list,
                              (ompi_free_list_item_t *) reg);
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

#if OMPI_CUDA_SUPPORT
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

    if(OMPI_SUCCESS != mca_mpool_grdma_register(mpool, addr, size, flags, reg)) {
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
    mca_mpool_base_registration_t *grdma_reg;
    ompi_free_list_item_t *item;
    unsigned char *base, *bound;
    bool bypass_cache = !!(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS);
    int rc;

    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    /* if cache bypass is requested don't use the cache */
    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
                                            mca_mpool_base_page_size_log);
    if (!opal_list_is_empty (&mpool_grdma->pool->gc_list))
        do_unregistration_gc(mpool);

    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(flags & MCA_MPOOL_FLAGS_PERSIST) && !bypass_cache) {
        /* check to see if memory is registered */
        mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
        if(*reg != NULL &&
           (mca_mpool_grdma_component.leave_pinned ||
            ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
            ((*reg)->base == base && (*reg)->bound == bound))) {
            if(0 == (*reg)->ref_count &&
               mca_mpool_grdma_component.leave_pinned) {
                opal_list_remove_item(&mpool_grdma->pool->lru_list,
                                      (opal_list_item_t*)(*reg));
            }
            mpool_grdma->stat_cache_hit++;
            (*reg)->ref_count++;
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return OMPI_SUCCESS;
        }

        mpool_grdma->stat_cache_miss++;
        *reg = NULL; /* in case previous find found something */

        /* If no suitable registration is in cache and leave_pinned isn't
         * set and size of registration cache is unlimited don't use the cache.
         * This is optimisation in case limit is not set. If limit is set we
         * have to put registration into the cache to determine when we hit
         * memory registration limit.
         * NONE: cache is still used for persistent registrations so previous
         * find can find something */
        if(!mca_mpool_grdma_component.leave_pinned &&
           mca_mpool_grdma_component.rcache_size_limit == 0) {
            bypass_cache = true;
        }
    }

    OMPI_FREE_LIST_GET(&mpool_grdma->reg_list, item, rc);
    if(OMPI_SUCCESS != rc) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return rc;
    }
    grdma_reg = (mca_mpool_base_registration_t*)item;

    grdma_reg->mpool = mpool;
    grdma_reg->base = base;
    grdma_reg->bound = bound;
    grdma_reg->flags = flags;

    if (false == bypass_cache) {
        rc = mpool->rcache->rcache_insert(mpool->rcache, grdma_reg, 0);

        if (OPAL_UNLIKELY(rc != OMPI_SUCCESS)) {
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            OMPI_FREE_LIST_RETURN(&mpool_grdma->reg_list, item);
            return rc;
        }
    }

    while (OMPI_ERR_OUT_OF_RESOURCE ==
           (rc = mpool_grdma->resources.register_mem(mpool_grdma->resources.reg_data,
                                                     base, bound - base + 1, grdma_reg))) {
        /* try to remove one unused reg and retry */
        if (!mca_mpool_grdma_evict (mpool)) {
            break;
        }
    }

    if (OPAL_UNLIKELY(rc != OMPI_SUCCESS)) {
        if (false == bypass_cache) {
            mpool->rcache->rcache_delete(mpool->rcache, grdma_reg);
        }
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN(&mpool_grdma->reg_list, item);
        return rc;
    }

    *reg = grdma_reg;
    (*reg)->ref_count++;
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);
    return OMPI_SUCCESS;
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

    rc = mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
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

static inline bool registration_is_cachebale(mca_mpool_base_registration_t *reg)
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
    int rc = OMPI_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg->ref_count--;
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OMPI_SUCCESS;
    }

    if(registration_is_cachebale(reg)) {
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
    int reg_cnt, i, rc = OMPI_SUCCESS;

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
                rc = OMPI_ERROR; /* tell caller that something was wrong */
            } else {
                opal_list_remove_item(&mpool_grdma->pool->lru_list,(opal_list_item_t *) regs[i]);
                opal_list_append(&mpool_grdma->pool->gc_list, (opal_list_item_t *) regs[i]);
            }
        }
    } while(reg_cnt == GRDMA_MPOOL_NREGS);

    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

void mca_mpool_grdma_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_grdma_module_t *mpool_grdma = (mca_mpool_grdma_module_t*)mpool;
    mca_mpool_base_registration_t *regs[GRDMA_MPOOL_NREGS];
    int reg_cnt, i;

    /* Statistic */
    if (true == mca_mpool_grdma_component.print_stats) {
        opal_output(0, "%s grdma: stats "
                "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
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
}

int mca_mpool_grdma_ft_event(int state) {
    return OMPI_SUCCESS;
}
