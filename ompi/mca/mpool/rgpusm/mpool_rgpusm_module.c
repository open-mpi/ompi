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
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file:
 *
 * This memory pool is used for getting the memory handle of remote
 * GPU memory when using CUDA.  Hence, the name is "rgpusm" for "remote
 * CUDA" GPU memory.  There is a cache that can be used to store the
 * remote handles in case they are reused to save on the registration
 * cost as that can be expensive, on the order of 100 usecs.  The
 * cache can also be used just to track how many handles are in use at
 * a time.  It is best to look at this with the three different
 * scenarios that are possible.
 * 1. mpool_rgpusm_leave_pinned=0, cache_size=unlimited
 * 2. mpool_rgpusm_leave_pinned=0, cache_size=limited
 * 3. mpool_rgpusm_leave_pinned=1, cache_size=unlimited (default)
 * 4. mpool_rgpusm_leave_pinned=1, cache_size=limited.
 * 
 * Case 1: The cache is unused and remote memory is registered and
 * unregistered for each transaction.  The amount of outstanding
 * registered memory is unlimited.
 * Case 2: The cache keeps track of how much memory is registered at a
 * time.  Since leave pinned is 0, any memory that is registered is in
 * use.  If the amount to register exceeds the amount, we will error
 * out.  This could be handled more gracefully, but this is not a
 * common way to run, so we will leave as is.
 * Case 3: The cache is needed to track current and past transactions.
 * However, there is no limit on the number that can be stored.
 * Therefore, once memory enters the cache, and gets registered, it
 * stays that way forever.
 * Case 4: The cache is needed to track current and past transactions.
 * In addition, a list of most recently used (but no longer in use)
 * registrations is stored so that it can be used to evict
 * registrations from the cache.  In addition, these registrations are
 * deregistered.
 * 
 * I also want to capture how we can run into the case where we do not
 * find something in the cache, but when we try to register it, we get
 * an error back from the CUDA library saying the memory is in use.
 * This can happen in the following scenario.  The application mallocs
 * a buffer of size 32K.  The library loads this in the cache and
 * registers it.  The application then frees the buffer.  It then
 * mallocs a buffer of size 64K.  This malloc returns the same base
 * address as the first 32K allocation.  The library searches the
 * cache, but since the size is larger than the original allocation it
 * does not find the registration.  It then attempts to register this.
 * The CUDA library returns an error saying it is already mapped.  To
 * handle this, we return an error of OMPI_ERR_WOULD_BLOCK to the
 * memory pool.  The memory pool then looks for the registration based
 * on the base address and a size of 4.  We use the small size to make
 * sure that we find the registration.  This registration is evicted,
 * and we try to register again.
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "opal/align.h"
#include "ompi/mca/mpool/rgpusm/mpool_rgpusm.h"
#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/rte/rte.h" 
#include "ompi/runtime/params.h"
#include "ompi/mca/common/cuda/common_cuda.h"


/* A hack so that page alignment is disabled in my instantiation of
 * the rcache.  This needs to be fixed. */
static size_t saved_page_size;
#define SET_PAGE_ALIGNMENT_TO_ZERO() \
    saved_page_size = mca_mpool_base_page_size_log; \
    mca_mpool_base_page_size_log = 0;

#define RESTORE_PAGE_ALIGNMENT() \
    mca_mpool_base_page_size_log = saved_page_size;

static inline bool mca_mpool_rgpusm_deregister_lru (mca_mpool_base_module_t *mpool) {
    mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t *) mpool;
    mca_mpool_base_registration_t *old_reg;
    int rc;

    /* Remove the registration from the cache and list before
       deregistering the memory */
    old_reg = (mca_mpool_base_registration_t*)
        opal_list_remove_first (&mpool_rgpusm->lru_list);
    if (NULL == old_reg) {
        return false;
    }

    mpool->rcache->rcache_delete(mpool->rcache, old_reg);

    /* Drop the rcache lock while we deregister the memory */
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
    assert(old_reg->ref_count == 0);
    rc = mpool_rgpusm->resources.deregister_mem(mpool_rgpusm->resources.reg_data,
                                                old_reg);
    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    /* This introduces a potential leak of registrations if
       the deregistration fails to occur as we no longer have
       a reference to it. Is this possible? */
    if (OMPI_SUCCESS != rc) {
        return false;
    }

    OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list,
                          (ompi_free_list_item_t*)old_reg);
    mpool_rgpusm->stat_evicted++;

    return true;
}


/*
 *  Initializes the mpool module.
 */
void mca_mpool_rgpusm_module_init(mca_mpool_rgpusm_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_rgpusm_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = NULL;
    mpool->super.mpool_realloc = NULL;
    mpool->super.mpool_free = mca_mpool_rgpusm_free;
    mpool->super.mpool_register = mca_mpool_rgpusm_register;
    mpool->super.mpool_find = mca_mpool_rgpusm_find;
    mpool->super.mpool_deregister = mca_mpool_rgpusm_deregister;
    mpool->super.mpool_release_memory = NULL;
    mpool->super.mpool_finalize = mca_mpool_rgpusm_finalize;
    mpool->super.mpool_ft_event = mca_mpool_rgpusm_ft_event;
    mpool->super.rcache =
        mca_rcache_base_module_create(mca_mpool_rgpusm_component.rcache_name);
    mpool->super.flags = 0;

    mpool->resources.reg_data = NULL;
    mpool->resources.sizeof_reg = sizeof(struct mca_mpool_common_cuda_reg_t);
    mpool->resources.register_mem = cuda_openmemhandle;
    mpool->resources.deregister_mem = cuda_closememhandle;

    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t);
    ompi_free_list_init_new(&mpool->reg_list, mpool->resources.sizeof_reg,
            opal_cache_line_size,
            OBJ_CLASS(mca_mpool_base_registration_t), 
            0,opal_cache_line_size,
            0, -1, 32, NULL);
    OBJ_CONSTRUCT(&mpool->lru_list, opal_list_t);
    mpool->stat_cache_hit = mpool->stat_cache_miss = mpool->stat_evicted = 0;
    mpool->stat_cache_found = mpool->stat_cache_notfound = 0;
    mpool->stat_cache_valid = mpool->stat_cache_invalid = 0;

}

/*
 * This function opens and handle using the handle that was received
 * from the remote memory.  It uses the addr and size of the remote
 * memory for caching the registration.
 */
int mca_mpool_rgpusm_register(mca_mpool_base_module_t *mpool, void *addr,
                             size_t size, uint32_t flags,
                             mca_mpool_base_registration_t **reg)
{
    mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t*)mpool;
    mca_mpool_common_cuda_reg_t *rgpusm_reg;
    mca_mpool_common_cuda_reg_t *rget_reg;
    ompi_free_list_item_t *item;
    int rc;
    int mypeer;  /* just for debugging */

    /* In order to preserve the signature of the mca_mpool_rgpusm_register
     * function, we are using the **reg variable to not only get back the
     * registration information, but to hand in the memory handle received
     * from the remote side. */
    rget_reg = (mca_mpool_common_cuda_reg_t *)*reg;

    mypeer = flags;
    flags = 0;
    /* No need to support MCA_MPOOL_FLAGS_CACHE_BYPASS in here. It is not used. */
    assert(0 == (flags & MCA_MPOOL_FLAGS_CACHE_BYPASS));

    /* This chunk of code handles the case where leave pinned is not
     * set and we do not use the cache.  This is not typically how we
     * will be running.  This means that one can have an unlimited
     * number of registrations occuring at the same time.  Since we
     * are not leaving the registrations pinned, the number of
     * registrations is unlimited and there is no need for a cache. */
    if(!mca_mpool_rgpusm_component.leave_pinned && 0 == mca_mpool_rgpusm_component.rcache_size_limit) {
        OMPI_FREE_LIST_GET_MT(&mpool_rgpusm->reg_list, item);
        if(NULL == item) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        rgpusm_reg = (mca_mpool_common_cuda_reg_t*)item;
        rgpusm_reg->base.mpool = mpool;
        rgpusm_reg->base.base = addr;
        rgpusm_reg->base.bound = (unsigned char *)addr + size - 1;;
        rgpusm_reg->base.flags = flags;

        /* Copy the memory handle received into the registration */
        memcpy(rgpusm_reg->memHandle, rget_reg->memHandle, sizeof(rget_reg->memHandle));

        /* The rget_reg registration is holding the memory handle needed
         * to register the remote memory.  This was received from the remote
         * process.  A pointer to the memory is returned in the alloc_base field. */
        rc = mpool_rgpusm->resources.register_mem(addr, size,
                                                 (mca_mpool_base_registration_t *)rgpusm_reg,
                                                 (mca_mpool_base_registration_t *)rget_reg);

        /* This error should not happen with no cache in use. */
        assert(OMPI_ERR_WOULD_BLOCK != rc);

        if(rc != OMPI_SUCCESS) {
            OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list, item);
            return rc;
        }
        rgpusm_reg->base.ref_count++;
        *reg = (mca_mpool_base_registration_t *)rgpusm_reg;
        return OMPI_SUCCESS;
    }

    /* Check to see if memory is registered and stored in the cache. */
    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    SET_PAGE_ALIGNMENT_TO_ZERO();
    mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
    RESTORE_PAGE_ALIGNMENT();

    /* If *reg is not NULL, we have a registration.  Let us see if the
     * memory handle matches the one we were looking for.  If not, the
     * registration is invalid and needs to be removed. This happens
     * if memory was allocated, freed, and allocated again and ends up
     * with the same virtual address and within the limits of the
     * previous registration.  The memory handle check will catch that
     * scenario as the handles have unique serial numbers.  */
    if (*reg != NULL) {
        mpool_rgpusm->stat_cache_hit++;
        opal_output_verbose(10, mca_mpool_rgpusm_component.output,
                            "RGPUSM: Found addr=%p,size=%d (base=%p,size=%d) in cache",
                            addr, (int)size, (*reg)->base,
                            (int)((*reg)->bound - (*reg)->base));

        if (mca_common_cuda_memhandle_matches((mca_mpool_common_cuda_reg_t *)*reg, rget_reg)) {
            /* Registration matches what was requested.  All is good. */
            mpool_rgpusm->stat_cache_valid++;
        } else {
            /* This is an old registration.  Need to boot it. */
            opal_output_verbose(10, mca_mpool_rgpusm_component.output,
                                "RGPUSM: Mismatched Handle: Evicting/unregistering "
                                "addr=%p,size=%d (base=%p,size=%d) from cache",
                                addr, (int)size, (*reg)->base,
                                (int)((*reg)->bound - (*reg)->base));

            /* The ref_count has to be zero as this memory cannot possibly
             * be in use.  Assert on that just to make sure. */
            assert(0 == (*reg)->ref_count);
            if (mca_mpool_rgpusm_component.leave_pinned) {
                opal_list_remove_item(&mpool_rgpusm->lru_list,
                                      (opal_list_item_t*)(*reg));
            }

            /* Bump the reference count to keep things copacetic in deregister */
            (*reg)->ref_count++;
            /* Invalidate the registration so it will get booted out. */
            (*reg)->flags |= MCA_MPOOL_FLAGS_INVALID;
            mca_mpool_rgpusm_deregister(mpool, *reg);
            *reg = NULL;
            mpool_rgpusm->stat_cache_invalid++;
        }
    } else {
        /* Nothing was found in the cache. */
        mpool_rgpusm->stat_cache_miss++;
    }

    /* If we have a registration here, then we know it is valid. */
    if (*reg != NULL) {
        opal_output_verbose(10, mca_mpool_rgpusm_component.output,
                            "RGPUSM: CACHE HIT is good: ep=%d, addr=%p, size=%d in cache",
                            mypeer, addr, (int)size);

        /* When using leave pinned, we keep an LRU list. */
        if ((0 == (*reg)->ref_count) && mca_mpool_rgpusm_component.leave_pinned) {
            opal_output_verbose(20, mca_mpool_rgpusm_component.output,
                                "RGPUSM: POP OFF LRU: ep=%d, addr=%p, size=%d in cache",
                                mypeer, addr, (int)size);
            opal_list_remove_item(&mpool_rgpusm->lru_list,
                                  (opal_list_item_t*)(*reg));
        }
        (*reg)->ref_count++;
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        opal_output(-1, "reg->ref_count=%d", (int)(*reg)->ref_count);
        opal_output_verbose(80, mca_mpool_rgpusm_component.output,
                           "RGPUSM: Found entry in cache addr=%p, size=%d", addr, (int)size);
        return OMPI_SUCCESS;
    }

    /* If we are here, then we did not find a registration, or it was invalid,
     * so this is a new one, and we are going to use the cache. */
    assert(NULL == *reg);
    opal_output_verbose(10, mca_mpool_rgpusm_component.output,
                        "RGPUSM: New registration ep=%d, addr=%p, size=%d. Need to register and insert in cache",
                         mypeer, addr, (int)size);

    OMPI_FREE_LIST_GET_MT(&mpool_rgpusm->reg_list, item);
    if(NULL == item) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    rgpusm_reg = (mca_mpool_common_cuda_reg_t*)item;

    rgpusm_reg->base.mpool = mpool;
    rgpusm_reg->base.base = addr;
    rgpusm_reg->base.bound = (unsigned char *)addr + size - 1;
    rgpusm_reg->base.flags = flags;

    /* Need the memory handle saved in the registration */
    memcpy(rgpusm_reg->memHandle, rget_reg->memHandle, sizeof(rget_reg->memHandle));

    /* Actually register the memory, which opens the memory handle.
     * Need to do this prior to putting in the cache as the base and
     * bound values may be changed by the registration.  The memory
     * associated with the handle comes back in the alloc_base
     * value. */
    rc = mpool_rgpusm->resources.register_mem(addr, size, (mca_mpool_base_registration_t *)rgpusm_reg,
                                             (mca_mpool_base_registration_t *)rget_reg);
    /* There is a chance we can get the OMPI_ERR_WOULD_BLOCK from the
     * CUDA codes attempt to register the memory.  The case that this
     * can happen is as follows.  A block of memory is registered.
     * Then the sending side frees the memory.  The sending side then
     * cuMemAllocs memory again and gets the same base
     * address. However, it cuMemAllocs a block that is larger than
     * the one in the cache.  The cache will return that memory is not
     * registered and call into CUDA to register it.  However, that
     * will fail with CUDA_ERROR_ALREADY_MAPPED.  Therefore we need to
     * boot that previous allocation out and deregister it first.
     */
    if (OMPI_ERR_WOULD_BLOCK == rc) {
        mca_mpool_base_registration_t *oldreg;

        SET_PAGE_ALIGNMENT_TO_ZERO();
        /* Need to make sure it is at least 4 bytes in size  This will
         * ensure we get the hit in the cache. */
        mpool->rcache->rcache_find(mpool->rcache, addr, 4, &oldreg);
        RESTORE_PAGE_ALIGNMENT();

        /* For most cases, we will find a registration that overlaps.
         * Removal of it should allow the registration we are
         * attempting to succeed. */
        if (NULL != oldreg) {
            /* The ref_count has to be zero as this memory cannot
             * possibly be in use.  Assert on that just to make sure. */
            assert(0 == oldreg->ref_count);
            if (mca_mpool_rgpusm_component.leave_pinned) {
                opal_list_remove_item(&mpool_rgpusm->lru_list,
                                      (opal_list_item_t*)oldreg);
            }

            /* Bump the reference count to keep things copacetic in deregister */
            oldreg->ref_count++;
            /* Invalidate the registration so it will get booted out. */
            oldreg->flags |= MCA_MPOOL_FLAGS_INVALID;
            mca_mpool_rgpusm_deregister(mpool, oldreg);
            mpool_rgpusm->stat_evicted++;

            /* And try again.  This one usually works. */
            rc = mpool_rgpusm->resources.register_mem(addr, size, (mca_mpool_base_registration_t *)rgpusm_reg,
                                                      (mca_mpool_base_registration_t *)rget_reg);
        }

        /* There is a chance that another registration is blocking our
         * ability to register.  Check the rc to see if we still need
         * to try and clear out registrations. */
        while (OMPI_SUCCESS != rc) {
            if (true != mca_mpool_rgpusm_deregister_lru(mpool)) {
                rc = OMPI_ERROR;
                break;
            }
            /* Clear out one registration. */
            rc = mpool_rgpusm->resources.register_mem(addr, size, (mca_mpool_base_registration_t *)rgpusm_reg,
                                                      (mca_mpool_base_registration_t *)rget_reg);
        }
    }

    if(rc != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list, item);
        return rc;
    }

    opal_output_verbose(80, mca_mpool_rgpusm_component.output,
                        "RGPUSM: About to insert in rgpusm cache addr=%p, size=%d", addr, (int)size);
    SET_PAGE_ALIGNMENT_TO_ZERO();
    while((rc = mpool->rcache->rcache_insert(mpool->rcache, (mca_mpool_base_registration_t *)rgpusm_reg,
             mca_mpool_rgpusm_component.rcache_size_limit)) ==
            OMPI_ERR_TEMP_OUT_OF_RESOURCE) {
        opal_output(-1, "No room in the cache - boot one out");
        if (!mca_mpool_rgpusm_deregister_lru(mpool)) {
            break;
        }
    }
    RESTORE_PAGE_ALIGNMENT();

    if(rc != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list, item);
        /* We cannot recover from this.  We can be here if the size of
         * the cache is smaller than the amount of memory we are
         * trying to register in a single transfer.  In that case, rc
         * is MPI_ERR_OUT_OF_RESOURCES, but everything is stuck at
         * that point.  Therefore, just error out completely.
         */
        return OMPI_ERROR;
    }

    rgpusm_reg->base.ref_count++;
    *reg = (mca_mpool_base_registration_t *)rgpusm_reg;
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);
    return OMPI_SUCCESS;
}


/**
  * free function
  */
void mca_mpool_rgpusm_free(mca_mpool_base_module_t *mpool, void *addr,
                         mca_mpool_base_registration_t *registration)
{
    void *alloc_base = registration->alloc_base;
    mca_mpool_rgpusm_deregister(mpool, registration);
    free(alloc_base);
}

int mca_mpool_rgpusm_find(struct mca_mpool_base_module_t *mpool, void *addr,
        size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t*)mpool;
    int rc;
    unsigned char *base, *bound;

    base = addr;
    bound = base + size - 1; /* To keep cache hits working correctly */

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    opal_output(-1, "Looking for addr=%p, size=%d", addr, (int)size);
    SET_PAGE_ALIGNMENT_TO_ZERO();
    rc = mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
    RESTORE_PAGE_ALIGNMENT();
    if(*reg != NULL && mca_mpool_rgpusm_component.leave_pinned) {
        if(0 == (*reg)->ref_count && mca_mpool_rgpusm_component.leave_pinned) {
            opal_list_remove_item(&mpool_rgpusm->lru_list, (opal_list_item_t*)(*reg));
        }
        mpool_rgpusm->stat_cache_found++;
        (*reg)->ref_count++;
    } else {
        mpool_rgpusm->stat_cache_notfound++;
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

static inline bool registration_is_cachebale(mca_mpool_base_registration_t *reg)
{
     return !(reg->flags &
             (MCA_MPOOL_FLAGS_CACHE_BYPASS |
              MCA_MPOOL_FLAGS_INVALID));
}

int mca_mpool_rgpusm_deregister(struct mca_mpool_base_module_t *mpool,
                            mca_mpool_base_registration_t *reg)
{
    mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t*)mpool;
    int rc = OMPI_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg->ref_count--;
    opal_output(-1, "Deregister: reg->ref_count=%d", (int)reg->ref_count);
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OMPI_SUCCESS;
    }
    if(mca_mpool_rgpusm_component.leave_pinned && registration_is_cachebale(reg))
    {
        /* if leave_pinned is set don't deregister memory, but put it
         * on LRU list for future use */
        opal_list_prepend(&mpool_rgpusm->lru_list, (opal_list_item_t*)reg);
    } else {
        /* Remove from rcache first */
        if(!(reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS))
            mpool->rcache->rcache_delete(mpool->rcache, reg);

        /* Drop the rcache lock before deregistring the memory */
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

        {
             mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t *)mpool;

             assert(reg->ref_count == 0);
             rc = mpool_rgpusm->resources.deregister_mem(mpool_rgpusm->resources.reg_data,
                                                         reg);
         }

        OPAL_THREAD_LOCK(&mpool->rcache->lock);

        if(OMPI_SUCCESS == rc) {
            OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list,
                                  (ompi_free_list_item_t*)reg);
        }
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

    return rc;
}

#define RGPUSM_MPOOL_NREGS 100

void mca_mpool_rgpusm_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_rgpusm_module_t *mpool_rgpusm = (mca_mpool_rgpusm_module_t*)mpool;
    mca_mpool_base_registration_t *reg;
    mca_mpool_base_registration_t *regs[RGPUSM_MPOOL_NREGS];
    int reg_cnt, i;
    int rc;

    /* Statistic */
    if(true == mca_mpool_rgpusm_component.print_stats) {
        opal_output(0, "%s rgpusm: stats "
                "(hit/valid/invalid/miss/evicted): %d/%d/%d/%d/%d\n",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                mpool_rgpusm->stat_cache_hit, mpool_rgpusm->stat_cache_valid, 
                mpool_rgpusm->stat_cache_invalid, mpool_rgpusm->stat_cache_miss,
                mpool_rgpusm->stat_evicted);
    }

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, 0, (size_t)-1,
                regs, RGPUSM_MPOOL_NREGS);
        opal_output(-1, "Registration size at finalize = %d", reg_cnt);

        for(i = 0; i < reg_cnt; i++) {
            reg = regs[i];

            if(reg->ref_count) {
                reg->ref_count = 0; /* otherway dereg will fail on assert */
            } else if (mca_mpool_rgpusm_component.leave_pinned) {
                opal_list_remove_item(&mpool_rgpusm->lru_list,
                        (opal_list_item_t*)reg);
            }

            /* Remove from rcache first */
            mpool->rcache->rcache_delete(mpool->rcache, reg);

            /* Drop lock before deregistering memory */
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            assert(reg->ref_count == 0);
            rc = mpool_rgpusm->resources.deregister_mem(mpool_rgpusm->resources.reg_data,
                                                   reg);
            OPAL_THREAD_LOCK(&mpool->rcache->lock);

            if(rc != OMPI_SUCCESS) {
                /* Potentially lose track of registrations
                   do we have to put it back? */
                continue;
            }

            OMPI_FREE_LIST_RETURN_MT(&mpool_rgpusm->reg_list,
                                  (ompi_free_list_item_t*)reg);
        }
    } while(reg_cnt == RGPUSM_MPOOL_NREGS);

    OBJ_DESTRUCT(&mpool_rgpusm->lru_list);
    OBJ_DESTRUCT(&mpool_rgpusm->reg_list);
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

}

int mca_mpool_rgpusm_ft_event(int state) {
    return OMPI_SUCCESS;
}
