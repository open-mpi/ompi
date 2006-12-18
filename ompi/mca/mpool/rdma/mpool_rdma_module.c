/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/include/opal/align.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"

extern uint32_t mca_mpool_base_page_size;
extern uint32_t mca_mpool_base_page_size_log;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_rdma_module_init(mca_mpool_rdma_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_rdma_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = mca_mpool_rdma_alloc;
    mpool->super.mpool_realloc = mca_mpool_rdma_realloc;
    mpool->super.mpool_free = mca_mpool_rdma_free;
    mpool->super.mpool_register = mca_mpool_rdma_register;
    mpool->super.mpool_find = mca_mpool_rdma_find;
    mpool->super.mpool_deregister = mca_mpool_rdma_deregister;
    mpool->super.mpool_release_memory = mca_mpool_rdma_release_memory;
    if(mca_mpool_rdma_component.print_stats == true)
        mpool->super.mpool_finalize = mca_mpool_rdma_finalize;
    else
        mpool->super.mpool_finalize = NULL;
    mpool->super.rcache =
        mca_rcache_base_module_create(mca_mpool_rdma_component.rcache_name);
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM;

    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t);
    ompi_free_list_init(&mpool->reg_list, mpool->resources.sizeof_reg,
                        OBJ_CLASS(mca_mpool_base_registration_t), 0, -1, 32,
                        NULL);
    OBJ_CONSTRUCT(&mpool->mru_list, opal_list_t);
    mpool->stat_cache_hit = mpool->stat_cache_miss = mpool->stat_evicted = 0;
    mpool->stat_cache_found = mpool->stat_cache_notfound = 0;
}

static inline int dereg_mem(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t *)mpool;

    assert(reg->ref_count == 0);
    return mpool_rdma->resources.deregister_mem(mpool_rdma->resources.reg_data,
            reg);
}

/**
  * allocate function
  */
void* mca_mpool_rdma_alloc(mca_mpool_base_module_t *mpool, size_t size,
        size_t align, uint32_t flags, mca_mpool_base_registration_t **reg)
{
    void *base_addr, *addr;

    if(0 == align)
        align = mca_mpool_base_page_size;

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

    if(OMPI_SUCCESS != mca_mpool_rdma_register(mpool, addr, size, flags, reg)) {
        free(base_addr);
        return NULL;
    }
    (*reg)->alloc_base = base_addr;

    return addr;
}

static int register_cache_bypass(mca_mpool_base_module_t *mpool,
        void *addr, size_t size, uint32_t flags,
        mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *rdma_reg;
    ompi_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    base = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = up_align_addr( (void*) ((char*) addr + size - 1),
             mca_mpool_base_page_size_log);
    OMPI_FREE_LIST_GET(&mpool_rdma->reg_list, item, rc);
    if(OMPI_SUCCESS != rc) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return rc;
    }
    rdma_reg = (mca_mpool_base_registration_t*)item;

    rdma_reg->mpool = mpool;
    rdma_reg->base = base;
    rdma_reg->bound = bound;
    rdma_reg->flags = flags;

    rc = mpool_rdma->resources.register_mem(mpool_rdma->resources.reg_data,
            base, bound - base + 1, rdma_reg);

    if(rc != OMPI_SUCCESS) {
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    *reg = rdma_reg;
    (*reg)->ref_count++;
    return OMPI_SUCCESS;
}

/*
 * register memory
 */
int mca_mpool_rdma_register(mca_mpool_base_module_t *mpool, void *addr,
                              size_t size, uint32_t flags,
                              mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *rdma_reg;
    ompi_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    /* if cache bypass is requested don't use the cache */
    if(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS) {
        return register_cache_bypass(mpool, addr, size, flags, reg);
    }

    base = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = up_align_addr((void*)((char*) addr + size - 1),
             mca_mpool_base_page_size_log);
    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(flags & MCA_MPOOL_FLAGS_PERSIST)) {
        /* check to see if memory is registered */
        mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
        if(*reg != NULL &&
                (mca_mpool_rdma_component.leave_pinned ||
                 ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
                 ((*reg)->base == base && (*reg)->bound == bound))) {
            if(0 == (*reg)->ref_count &&
                    mca_mpool_rdma_component.leave_pinned) {
                opal_list_remove_item(&mpool_rdma->mru_list,
                        (opal_list_item_t*)(*reg));
            }
            mpool_rdma->stat_cache_hit++;
            (*reg)->ref_count++;
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return MPI_SUCCESS;
        }

        mpool_rdma->stat_cache_miss++;
        *reg = NULL; /* in case previous find found something */

        /* If no suitable registration is in cache and leave_pinned isn't
         * set and size of registration cache is unlimited don't use the cache.
         * This is optimisation in case limit is not set. If limit is set we
         * have to put registration into the cache to determine when we hit
         * memory registration limit.
         * NONE: cache is still used for persistent registrations so previous
         * find can find something */
        if(!mca_mpool_rdma_component.leave_pinned &&
                 mca_mpool_rdma_component.rcache_size_limit == 0) {
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return register_cache_bypass(mpool, addr, size, flags, reg);
        }
    }

    OMPI_FREE_LIST_GET(&mpool_rdma->reg_list, item, rc);
    if(OMPI_SUCCESS != rc) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return rc;
    }
    rdma_reg = (mca_mpool_base_registration_t*)item;

    rdma_reg->mpool = mpool;
    rdma_reg->base = base;
    rdma_reg->bound = bound;
    rdma_reg->flags = flags;

    while((rc = mpool->rcache->rcache_insert(mpool->rcache, rdma_reg,
             mca_mpool_rdma_component.rcache_size_limit)) ==
            OMPI_ERR_TEMP_OUT_OF_RESOURCE) {
        mca_mpool_base_registration_t *old_reg;
        /* try to remove one unused reg and retry */
        old_reg = (mca_mpool_base_registration_t*)
            opal_list_get_last(&mpool_rdma->mru_list);
        if(opal_list_get_end(&mpool_rdma->mru_list) !=
                (opal_list_item_t*)old_reg) {
            rc = dereg_mem(mpool, old_reg);
            if(MPI_SUCCESS == rc) {
                mpool->rcache->rcache_delete(mpool->rcache, old_reg);
                opal_list_remove_item(&mpool_rdma->mru_list,
                        (opal_list_item_t*)old_reg);
                OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                        (ompi_free_list_item_t*)old_reg);
                mpool_rdma->stat_evicted++;
            } else
                break;
        } else
            break;
    }

    if(rc != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    rc = mpool_rdma->resources.register_mem(mpool_rdma->resources.reg_data,
            base, bound - base + 1, rdma_reg);

    if(rc != OMPI_SUCCESS) {
        mpool->rcache->rcache_delete(mpool->rcache, rdma_reg);
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    *reg = rdma_reg;
    (*reg)->ref_count++;
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
    return OMPI_SUCCESS;
}


/**
  * realloc function
  */
void* mca_mpool_rdma_realloc(mca_mpool_base_module_t *mpool, void *addr,
    size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_base_registration_t *old_reg  = *reg;
    void *new_mem = mca_mpool_rdma_alloc(mpool, size, 0, old_reg->flags, reg);
    memcpy(new_mem, addr, old_reg->bound - old_reg->base + 1);
    mca_mpool_rdma_free(mpool, addr, old_reg);

    return new_mem;
}

/**
  * free function
  */
void mca_mpool_rdma_free(mca_mpool_base_module_t *mpool, void *addr,
                         mca_mpool_base_registration_t *registration)
{
    mca_mpool_rdma_deregister(mpool, registration);
    free(registration->alloc_base);
}

int mca_mpool_rdma_find(struct mca_mpool_base_module_t *mpool, void *addr,
        size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    int rc;
    unsigned char *base, *bound;

    base = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = up_align_addr((void*)((char*) addr + size - 1),
             mca_mpool_base_page_size_log);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    rc = mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
    if(*reg != NULL &&
            (mca_mpool_rdma_component.leave_pinned ||
             ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
             ((*reg)->base == base && (*reg)->bound == bound))) {
        assert(((void*)(*reg)->bound) >= addr);
        if(0 == (*reg)->ref_count &&
                mca_mpool_rdma_component.leave_pinned) {
            opal_list_remove_item(&mpool_rdma->mru_list,
                    (opal_list_item_t*)(*reg));
        }
        mpool_rdma->stat_cache_found++;
        (*reg)->ref_count++;
    } else {
        mpool_rdma->stat_cache_notfound++;
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

int mca_mpool_rdma_deregister(struct mca_mpool_base_module_t *mpool,
                            mca_mpool_base_registration_t *reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    int rc = OMPI_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg->ref_count--;
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OMPI_SUCCESS;
    }
    if(mca_mpool_rdma_component.leave_pinned &&
       !(reg->flags & (MCA_MPOOL_FLAGS_CACHE_BYPASS|MCA_MPOOL_FLAGS_PERSIST))) {
        /* if leave_pinned is set don't deregister memory, but put it
         * on MRU list for future use */
        opal_list_prepend(&mpool_rdma->mru_list, (opal_list_item_t*)reg);
    } else {
        rc = dereg_mem(mpool, reg);
        if(OMPI_SUCCESS == rc) {
            if(!(reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS))
                mpool->rcache->rcache_delete(mpool->rcache, reg);
            OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                    (ompi_free_list_item_t*)reg);
        }
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

int mca_mpool_rdma_release_memory(struct mca_mpool_base_module_t *mpool,
        void *base, size_t size)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *reg;
    ompi_pointer_array_t regs;
    int reg_cnt, i, err = 0;

    OBJ_CONSTRUCT(&regs, ompi_pointer_array_t);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, base, size, &regs);

    for(i = 0; i < reg_cnt; i++) {
        reg = (mca_mpool_base_registration_t*)
            ompi_pointer_array_get_item(&regs, i);

        if(0 == reg->ref_count) {
            if(dereg_mem(mpool, reg) != OMPI_SUCCESS) {
                err++;
                continue;
            }
        } else {
            /* remove registration from cache and wait for ref_count goes to
             * zero before unregister memory. Note that our registered memory
             * statistic can go wrong at this point, but it is better than
             * potential memory corruption. And we return error in this case to
             * the caller */
            reg->flags |= MCA_MPOOL_FLAGS_CACHE_BYPASS;
            err++; /* tell caller that something was wrong */
        }
        mpool->rcache->rcache_delete(mpool->rcache, reg);
        if(0 == reg->ref_count) {
            opal_list_remove_item(&mpool_rdma->mru_list,
                    (opal_list_item_t*)reg);
            OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                    (ompi_free_list_item_t*)reg);
        }
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
    ompi_pointer_array_remove_all(&regs);

    return err?OMPI_ERROR:OMPI_SUCCESS;
}

void mca_mpool_rdma_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    opal_output(0, "[%lu,%lu,%lu] rdma: stats "
            "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            mpool_rdma->stat_cache_hit, mpool_rdma->stat_cache_miss,
            mpool_rdma->stat_cache_found, mpool_rdma->stat_cache_notfound,
            mpool_rdma->stat_evicted);
}
