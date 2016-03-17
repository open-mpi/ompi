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

#include "opal/util/sys_limits.h"
#include "opal/align.h"
#include "rcache_grdma.h"


static int mca_rcache_grdma_register (mca_rcache_base_module_t *rcache, void *addr,
                                      size_t size, uint32_t flags, int32_t access_flags,
                                      mca_rcache_base_registration_t **reg);
static int mca_rcache_grdma_deregister (mca_rcache_base_module_t *rcache,
                                        mca_rcache_base_registration_t *reg);
static int mca_rcache_grdma_find (mca_rcache_base_module_t *rcache, void *addr,
                                  size_t size, mca_rcache_base_registration_t **reg);
static int mca_rcache_grdma_invalidate_range (mca_rcache_base_module_t *rcache, void *base,
                                              size_t size);
static void mca_rcache_grdma_finalize (mca_rcache_base_module_t *rcache);
static bool mca_rcache_grdma_evict (mca_rcache_base_module_t *rcache);

static inline bool registration_is_cacheable(mca_rcache_base_registration_t *reg)
{
    return (mca_rcache_grdma_component.leave_pinned &&
            !(reg->flags &
              (MCA_RCACHE_FLAGS_CACHE_BYPASS |
               MCA_RCACHE_FLAGS_PERSIST |
               MCA_RCACHE_FLAGS_INVALID)));
}

#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory(mca_rcache_base_module_t *rcache, void *addr, size_t size);
#endif /* OPAL_CUDA_GDR_SUPPORT */
static void mca_rcache_grdma_cache_contructor (mca_rcache_grdma_cache_t *cache)
{
    memset ((void *)((uintptr_t)cache + sizeof (cache->super)), 0, sizeof (*cache) - sizeof (cache->super));

    OBJ_CONSTRUCT(&cache->lru_list, opal_list_t);
    OBJ_CONSTRUCT(&cache->gc_list, opal_list_t);

    cache->vma_module = mca_rcache_base_vma_module_alloc ();
}

static void mca_rcache_grdma_cache_destructor (mca_rcache_grdma_cache_t *cache)
{
    OBJ_DESTRUCT(&cache->lru_list);
    OBJ_DESTRUCT(&cache->gc_list);
    if (cache->vma_module) {
        OBJ_RELEASE(cache->vma_module);
    }

    free (cache->cache_name);
}

OBJ_CLASS_INSTANCE(mca_rcache_grdma_cache_t, opal_list_item_t,
                   mca_rcache_grdma_cache_contructor,
                   mca_rcache_grdma_cache_destructor);

/*
 *  Initializes the rcache module.
 */
void mca_rcache_grdma_module_init(mca_rcache_grdma_module_t* rcache, mca_rcache_grdma_cache_t *cache)
{
    OBJ_RETAIN(cache);
    rcache->cache = cache;

    rcache->super.rcache_component = &mca_rcache_grdma_component.super;
    rcache->super.rcache_register = mca_rcache_grdma_register;
    rcache->super.rcache_find = mca_rcache_grdma_find;
    rcache->super.rcache_deregister = mca_rcache_grdma_deregister;
    rcache->super.rcache_invalidate_range = mca_rcache_grdma_invalidate_range;
    rcache->super.rcache_finalize = mca_rcache_grdma_finalize;
    rcache->super.rcache_evict = mca_rcache_grdma_evict;

    rcache->stat_cache_hit = rcache->stat_cache_miss = rcache->stat_evicted = 0;
    rcache->stat_cache_found = rcache->stat_cache_notfound = 0;

    OBJ_CONSTRUCT(&rcache->reg_list, opal_free_list_t);
    opal_free_list_init (&rcache->reg_list, rcache->resources.sizeof_reg,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_rcache_base_registration_t),
                         0, opal_cache_line_size, 0, -1, 32, NULL, 0,
                         NULL, NULL, NULL);
}

static inline int dereg_mem(mca_rcache_base_registration_t *reg)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t *) reg->rcache;
    int rc;

    if(!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
        mca_rcache_base_vma_delete (rcache_grdma->cache->vma_module, reg);
    }

    rc = rcache_grdma->resources.deregister_mem (rcache_grdma->resources.reg_data, reg);
    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_free_list_return (&rcache_grdma->reg_list,
                               (opal_free_list_item_t *) reg);
    }

    return rc;
}

/* This function must be called with the rcache lock held */
static inline void do_unregistration_gc (mca_rcache_base_module_t *rcache)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t *) rcache;
    opal_list_item_t *item;

    /* Remove registration from garbage collection list
       before deregistering it */
    while (NULL !=
           (item = opal_list_remove_first(&rcache_grdma->cache->gc_list))) {
        dereg_mem((mca_rcache_base_registration_t *) item);
    }
}

static inline bool mca_rcache_grdma_evict_lru_local (mca_rcache_grdma_cache_t *cache)
{
    mca_rcache_grdma_module_t *rcache_grdma;
    mca_rcache_base_registration_t *old_reg;

    old_reg = (mca_rcache_base_registration_t *)
        opal_list_remove_first (&cache->lru_list);
    if (NULL == old_reg) {
        return false;
    }

    rcache_grdma = (mca_rcache_grdma_module_t *) old_reg->rcache;

    (void) dereg_mem (old_reg);

    rcache_grdma->stat_evicted++;

    return true;
}

static bool mca_rcache_grdma_evict (mca_rcache_base_module_t *rcache)
{
    return mca_rcache_grdma_evict_lru_local (((mca_rcache_grdma_module_t *) rcache)->cache);
}

/*
 * register memory
 */
static int mca_rcache_grdma_register (mca_rcache_base_module_t *rcache, void *addr,
                                      size_t size, uint32_t flags, int32_t access_flags,
                                      mca_rcache_base_registration_t **reg)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t*)rcache;
    const bool bypass_cache = !!(flags & MCA_RCACHE_FLAGS_CACHE_BYPASS);
    const bool persist = !!(flags & MCA_RCACHE_FLAGS_PERSIST);
    mca_rcache_base_registration_t *grdma_reg;
    opal_free_list_item_t *item;
    unsigned char *base, *bound;
    unsigned int page_size = opal_getpagesize ();
    int rc;

    OPAL_THREAD_LOCK(&rcache_grdma->cache->vma_module->vma_lock);

    *reg = NULL;

    /* if cache bypass is requested don't use the cache */
    base = OPAL_DOWN_ALIGN_PTR(addr, page_size, unsigned char *);
    bound = OPAL_ALIGN_PTR((intptr_t) addr + size, page_size, unsigned char *) - 1;
    if (!opal_list_is_empty (&rcache_grdma->cache->gc_list))
        do_unregistration_gc(rcache);

#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_RCACHE_FLAGS_CUDA_GPU_MEM) {
        size_t psize;
        mca_common_cuda_get_address_range(&base, &psize, addr);
        bound = base + psize - 1;
        /* Check to see if this memory is in the cache and if it has been freed. If so,
         * this call will boot it out of the cache. */
        check_for_cuda_freed_memory(rcache, base, psize);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(bypass_cache || persist)) {
        /* check to see if memory is registered */
        mca_rcache_base_vma_find (rcache_grdma->cache->vma_module, base, bound - base + 1, &grdma_reg);
        if (grdma_reg && !(flags & MCA_RCACHE_FLAGS_INVALID)) {
            if (OPAL_UNLIKELY((access_flags & grdma_reg->access_flags) != access_flags)) {
                access_flags |= grdma_reg->access_flags;

                if (0 != grdma_reg->ref_count) {
                    if (!(grdma_reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
                        mca_rcache_base_vma_delete (rcache_grdma->cache->vma_module, grdma_reg);
                    }

                    /* mark the registration to go away when it is deregistered */
                    grdma_reg->flags |= MCA_RCACHE_FLAGS_INVALID | MCA_RCACHE_FLAGS_CACHE_BYPASS;
                } else {
                    if (registration_is_cacheable (grdma_reg)) {
                        /* pull the item out of the lru */
                        opal_list_remove_item (&rcache_grdma->cache->lru_list, (opal_list_item_t *) grdma_reg);
                    }

                    (void) dereg_mem (grdma_reg);
                }
            } else {
                *reg = grdma_reg;
                if (0 == grdma_reg->ref_count) {
                    /* Leave pinned must be set for this to still be in the rcache. */
                    opal_list_remove_item(&rcache_grdma->cache->lru_list,
                                          (opal_list_item_t *) grdma_reg);
                }

                /* This segment fits fully within an existing segment. */
                rcache_grdma->stat_cache_hit++;
                grdma_reg->ref_count++;
                OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
                return OPAL_SUCCESS;
            }
        }

        rcache_grdma->stat_cache_miss++;

        /* Unless explicitly requested by the caller always store the
         * registration in the rcache. This will speed up the case where
         * no leave pinned protocol is in use but the same segment is in
         * use in multiple simultaneous transactions. We used to set bypass_cache
         * here is !mca_rcache_grdma_component.leave_pinned. */
    }

    item = opal_free_list_get (&rcache_grdma->reg_list);
    if(NULL == item) {
        OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    grdma_reg = (mca_rcache_base_registration_t*)item;

    grdma_reg->rcache = rcache;
    grdma_reg->base = base;
    grdma_reg->bound = bound;
    grdma_reg->flags = flags;
    grdma_reg->access_flags = access_flags;
#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_RCACHE_FLAGS_CUDA_GPU_MEM) {
        mca_common_cuda_get_buffer_id(grdma_reg);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    if (false == bypass_cache) {
        rc = mca_rcache_base_vma_insert (rcache_grdma->cache->vma_module, grdma_reg, 0);

        if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
            OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
            opal_free_list_return (&rcache_grdma->reg_list, item);
            return rc;
        }
    }

    while (OPAL_ERR_OUT_OF_RESOURCE ==
           (rc = rcache_grdma->resources.register_mem(rcache_grdma->resources.reg_data,
                                                     base, bound - base + 1, grdma_reg))) {
        /* try to remove one unused reg and retry */
        if (!mca_rcache_grdma_evict (rcache)) {
            break;
        }
    }

    if (OPAL_UNLIKELY(rc != OPAL_SUCCESS)) {
        if (false == bypass_cache) {
            mca_rcache_base_vma_delete (rcache_grdma->cache->vma_module, grdma_reg);
        }
        OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
        opal_free_list_return (&rcache_grdma->reg_list, item);
        return rc;
    }

    *reg = grdma_reg;
    (*reg)->ref_count++;
    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);

    return OPAL_SUCCESS;
}

static int mca_rcache_grdma_find (mca_rcache_base_module_t *rcache, void *addr,
                                  size_t size, mca_rcache_base_registration_t **reg)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t*)rcache;
    unsigned long page_size = opal_getpagesize ();
    unsigned char *base, *bound;
    int rc;

    base = OPAL_DOWN_ALIGN_PTR(addr, page_size, unsigned char *);
    bound = OPAL_ALIGN_PTR((intptr_t) addr + size - 1, page_size, unsigned char *);

    OPAL_THREAD_LOCK(&rcache_grdma->cache->vma_module->vma_lock);

    rc = mca_rcache_base_vma_find (rcache_grdma->cache->vma_module, base, bound - base + 1, reg);
    if(NULL != *reg &&
            (mca_rcache_grdma_component.leave_pinned ||
             ((*reg)->flags & MCA_RCACHE_FLAGS_PERSIST) ||
             ((*reg)->base == base && (*reg)->bound == bound))) {
        assert(((void*)(*reg)->bound) >= addr);
        if(0 == (*reg)->ref_count &&
                mca_rcache_grdma_component.leave_pinned) {
            opal_list_remove_item(&rcache_grdma->cache->lru_list,
                                  (opal_list_item_t*)(*reg));
        }
        rcache_grdma->stat_cache_found++;
        (*reg)->ref_count++;
    } else {
        rcache_grdma->stat_cache_notfound++;
    }

    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);

    return rc;
}

static int mca_rcache_grdma_deregister (mca_rcache_base_module_t *rcache,
                                        mca_rcache_base_registration_t *reg)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t *) rcache;
    int rc = OPAL_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&rcache_grdma->cache->vma_module->vma_lock);
    reg->ref_count--;
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
        return OPAL_SUCCESS;
    }

    if (registration_is_cacheable(reg)) {
        opal_list_append(&rcache_grdma->cache->lru_list, (opal_list_item_t *) reg);
    } else {
        rc = dereg_mem (reg);
    }
    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);

    return rc;
}

#define GRDMA_RCACHE_NREGS 100

static int mca_rcache_grdma_invalidate_range (mca_rcache_base_module_t *rcache,
                                              void *base, size_t size)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t *) rcache;
    mca_rcache_base_registration_t *regs[GRDMA_RCACHE_NREGS];
    int reg_cnt, i, rc = OPAL_SUCCESS;

    OPAL_THREAD_LOCK(&rcache_grdma->cache->vma_module->vma_lock);
    do {
        reg_cnt = mca_rcache_base_vma_find_all (rcache_grdma->cache->vma_module, base,
                                                size, regs, GRDMA_RCACHE_NREGS);

        for(i = 0 ; i < reg_cnt ; ++i) {
            regs[i]->flags |= MCA_RCACHE_FLAGS_INVALID;
            if (regs[i]->ref_count) {
                /* memory is being freed, but there are registration in use that
                 * covers the memory. This can happen even in a correct program,
                 * but may also be an user error. We can't tell. Mark the
                 * registration as invalid. It will not be used any more and
                 * will be unregistered when ref_count will become zero */
                rc = OPAL_ERROR; /* tell caller that something was wrong */
            } else {
                opal_list_remove_item(&rcache_grdma->cache->lru_list,(opal_list_item_t *) regs[i]);
                opal_list_append(&rcache_grdma->cache->gc_list, (opal_list_item_t *) regs[i]);
            }
        }
    } while (reg_cnt == GRDMA_RCACHE_NREGS);

    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);

    return rc;
}

/* Make sure this registration request is not stale.  In other words, ensure
 * that we do not have a cuMemAlloc, cuMemFree, cuMemAlloc state.  If we do
 * kick out the regisrations and deregister.  This function needs to be called
 * with the rcache->vma_module->vma_lock held. */
#if OPAL_CUDA_GDR_SUPPORT
static int check_for_cuda_freed_memory (mca_rcache_base_module_t *rcache, void *addr, size_t size)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t *) rcache;
    mca_rcache_base_registration_t *regs[GRDMA_RCACHE_NREGS];
    int reg_cnt, i, rc = OPAL_SUCCESS;
    mca_rcache_base_registration_t *reg;

    mca_rcache_base_vma_find (rcache_grdma->cache->vma_module, addr, size, &reg);
    if (NULL == reg) {
        return OPAL_SUCCESS;
    }

    /* If not previously freed memory, just return 0 */
    if (!(mca_common_cuda_previously_freed_memory(reg))) {
        return OPAL_SUCCESS;
    }

    /* rcache->vma_module->rcache_dump_range(rcache->rcache, 0, (size_t)-1, "Before free"); */

    /* This memory has been freed.  Find all registrations and delete */
    do {
        reg_cnt = mca_rcache_base_vma_find_all (rcache_grdma->cache->vma_module, reg->base,
                                                reg->bound - reg->base + 1, regs,
                                                GRDMA_RCACHE_NREGS);
        for(i = 0 ; i < reg_cnt ; ++i) {
            regs[i]->flags |= MCA_RCACHE_FLAGS_INVALID;
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
                opal_list_remove_item(&rcache_grdma->cache->lru_list,(opal_list_item_t *) regs[i]);
                /* Now deregister.  Do not use gc_list as we need to kick this out now. */
                dereg_mem(regs[i]);
            }
        }
    } while(reg_cnt == GRDMA_RCACHE_NREGS);

    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);
    /* rcache->rcache->rcache_dump_range(rcache->rcache, 0, (size_t)-1, "After free");*/

    return rc;
}
#endif /* OPAL_CUDA_GDR_SUPPORT */

static void mca_rcache_grdma_finalize (mca_rcache_base_module_t *rcache)
{
    mca_rcache_grdma_module_t *rcache_grdma = (mca_rcache_grdma_module_t*)rcache;
    mca_rcache_base_registration_t *regs[GRDMA_RCACHE_NREGS];
    int reg_cnt, i;

    /* Statistic */
    if (true == mca_rcache_grdma_component.print_stats) {
        opal_output(0, "%s grdma: stats "
                "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                rcache_grdma->stat_cache_hit, rcache_grdma->stat_cache_miss,
                rcache_grdma->stat_cache_found, rcache_grdma->stat_cache_notfound,
                rcache_grdma->stat_evicted);
    }

    OPAL_THREAD_LOCK(&rcache_grdma->cache->vma_module->vma_lock);

    do_unregistration_gc(rcache);

    do {
        reg_cnt = mca_rcache_base_vma_find_all (rcache_grdma->cache->vma_module, 0, (size_t)-1,
                                                regs, GRDMA_RCACHE_NREGS);

        for (i = 0 ; i < reg_cnt ; ++i) {
            if (regs[i]->ref_count) {
                regs[i]->ref_count = 0; /* otherwise dereg will fail on assert */
            } else if (mca_rcache_grdma_component.leave_pinned) {
                opal_list_remove_item(&rcache_grdma->cache->lru_list,
                                      (opal_list_item_t *) regs[i]);
            }

	    (void) dereg_mem(regs[i]);
        }
    } while (reg_cnt == GRDMA_RCACHE_NREGS);

    OBJ_RELEASE(rcache_grdma->cache);

    OBJ_DESTRUCT(&rcache_grdma->reg_list);
    OPAL_THREAD_UNLOCK(&rcache_grdma->cache->vma_module->vma_lock);

    /* this rcache was allocated by grdma_init in rcache_grdma_component.c */
    free(rcache);
}
