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
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
#include "opal/align.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "mpool_udreg.h"
#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "ompi/mca/mpool/base/base.h"
#include "ompi/runtime/params.h"
#include "opal/include/opal_stdint.h"

#include <fcntl.h>

#include <udreg_pub.h>

#include <sys/mman.h>

static void *mca_mpool_udreg_reg_func (void *addr, uint64_t len, void *reg_context);
static uint32_t mca_mpool_udreg_dereg_func (void *device_data, void *dreg_context);

static void mca_mpool_udreg_hugepage_constructor (mca_mpool_udreg_hugepage_t *huge_page)
{
    memset ((char *)huge_page + sizeof(huge_page->super), 0, sizeof (*huge_page) - sizeof (huge_page->super));
    OBJ_CONSTRUCT(&huge_page->allocations, opal_list_t);
}

static void mca_mpool_udreg_hugepage_destructor (mca_mpool_udreg_hugepage_t *huge_page)
{
    opal_list_item_t *item;

    if (huge_page->path) {
        free (huge_page->path);
    }

    while (NULL != (item = opal_list_remove_first (&huge_page->allocations))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&huge_page->allocations);
}

OBJ_CLASS_INSTANCE(mca_mpool_udreg_hugepage_t, opal_list_item_t,
                   mca_mpool_udreg_hugepage_constructor,
                   mca_mpool_udreg_hugepage_destructor);

static void mca_mpool_udreg_hugepage_alloc_constructor (mca_mpool_udreg_hugepage_alloc_t *alloc)
{
    memset ((char *)alloc + sizeof(alloc->super), 0, sizeof (*alloc) - sizeof (alloc->super));
    alloc->fd = -1;
}

static void mca_mpool_udreg_hugepage_alloc_destructor (mca_mpool_udreg_hugepage_alloc_t *alloc)
{
    if (NULL != alloc->ptr) {
        munmap (alloc->ptr, alloc->size);
    }

    if (NULL == alloc->path) {
        return;
    }

    free (alloc->path);
}

OBJ_CLASS_INSTANCE(mca_mpool_udreg_hugepage_alloc_t, opal_list_item_t,
                   mca_mpool_udreg_hugepage_alloc_constructor,
                   mca_mpool_udreg_hugepage_alloc_destructor);


static mca_mpool_udreg_hugepage_t *udreg_find_matching_pagesize (size_t size) {
    mca_mpool_udreg_hugepage_t *huge_table;
    opal_list_item_t *item;

    for (item = opal_list_get_first (&mca_mpool_udreg_component.huge_pages) ;
         item != opal_list_get_end (&mca_mpool_udreg_component.huge_pages) ;
         item = opal_list_get_next (item)) {
        huge_table = (mca_mpool_udreg_hugepage_t *) item;

        if (huge_table->page_size == size) {
            return huge_table;
        }
    }

    return NULL;
}


/*
 *  Initializes the mpool module.
 */
int mca_mpool_udreg_module_init(mca_mpool_udreg_module_t* mpool)
{
    struct udreg_cache_attr cache_attr;
    int urc;

    mpool->super.mpool_component = &mca_mpool_udreg_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = mca_mpool_udreg_alloc;
    mpool->super.mpool_realloc = mca_mpool_udreg_realloc;
    mpool->super.mpool_free = mca_mpool_udreg_free;
    mpool->super.mpool_register = mca_mpool_udreg_register;
    mpool->super.mpool_find = mca_mpool_udreg_find;
    mpool->super.mpool_deregister = mca_mpool_udreg_deregister;
    /* This module relies on udreg for notification of memory release */
    mpool->super.mpool_release_memory = NULL;
    mpool->super.mpool_finalize = mca_mpool_udreg_finalize;
    mpool->super.mpool_ft_event = mca_mpool_udreg_ft_event;
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM | MCA_MPOOL_FLAGS_NO_HOOKS;

    if (4096 < mpool->resources.page_size) {
        mpool->huge_page = udreg_find_matching_pagesize (mpool->resources.page_size);
    } else {
        mpool->huge_page = NULL;
    }

    cache_attr.modes = 0;

    /* Create udreg cache */
    if (mpool->resources.use_kernel_cache) {
        cache_attr.modes |= UDREG_CC_MODE_USE_KERNEL_CACHE;
    }

    if (mpool->resources.use_evict_w_unreg) {
        cache_attr.modes |= UDREG_CC_MODE_USE_EVICT_W_UNREG;
    }

    if (mca_mpool_udreg_component.leave_pinned) {
        cache_attr.modes |= UDREG_CC_MODE_USE_LAZY_DEREG;
    }

    strncpy (cache_attr.cache_name, mpool->resources.pool_name, UDREG_MAX_CACHENAME_LEN);
    cache_attr.max_entries         = mpool->resources.max_entries;
    cache_attr.debug_mode          = 0;
    cache_attr.debug_rank          = 0;
    cache_attr.reg_context         = mpool;
    cache_attr.dreg_context        = mpool;
    cache_attr.destructor_context  = mpool;
    cache_attr.device_reg_func     = mca_mpool_udreg_reg_func;
    cache_attr.device_dereg_func   = mca_mpool_udreg_dereg_func;
    cache_attr.destructor_callback = NULL;

    /* attempt to create the udreg cache. this will fail if one already exists */
    (void) UDREG_CacheCreate (&cache_attr);

    urc = UDREG_CacheAccess (mpool->resources.pool_name, (udreg_cache_handle_t *) &mpool->udreg_handle);
    if (UDREG_RC_SUCCESS != urc) {
        return OMPI_ERROR;
    }

    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t);
    ompi_free_list_init_new(&mpool->reg_list, mpool->resources.sizeof_reg,
                            opal_cache_line_size,
                            OBJ_CLASS(mca_mpool_base_registration_t),
                            0, opal_cache_line_size, 0, -1, 32, NULL);

    return OMPI_SUCCESS;
}

/* udreg callback functions */
static void *mca_mpool_udreg_reg_func (void *addr, uint64_t len, void *reg_context)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t *) reg_context;
    mca_mpool_base_registration_t *udreg_reg;
    ompi_free_list_item_t *item;
    int rc;

    OMPI_FREE_LIST_GET_MT(&mpool_udreg->reg_list, item);
    if (NULL == item) {
        return NULL;
    }
    udreg_reg = (mca_mpool_base_registration_t *) item;

    udreg_reg->mpool = reg_context;
    udreg_reg->base  = addr;
    udreg_reg->bound = (void *)((uintptr_t) addr + len);

    rc = mpool_udreg->resources.register_mem(mpool_udreg->resources.reg_data,
                                             addr, len, udreg_reg);
    if (OMPI_SUCCESS != rc) {
        OMPI_FREE_LIST_RETURN_MT(&mpool_udreg->reg_list, item);
        udreg_reg = NULL;
    }

    return udreg_reg;
}

static uint32_t mca_mpool_udreg_dereg_func (void *device_data, void *dreg_context)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t *) dreg_context;
    mca_mpool_base_registration_t *udreg_reg = (mca_mpool_base_registration_t *) device_data;
    int rc;

    rc = mpool_udreg->resources.deregister_mem(mpool_udreg->resources.reg_data, udreg_reg);

    if (OPAL_LIKELY(OMPI_SUCCESS == rc)) {
        OMPI_FREE_LIST_RETURN_MT(&mpool_udreg->reg_list,
                              (ompi_free_list_item_t *) udreg_reg);
    }
    /* might be worth printing out a warning if an error occurs here */

    return 0;
}

/* */

static int mca_mpool_udreg_alloc_huge (mca_mpool_udreg_module_t *mpool, size_t size,
                                       void **addr, void **base_addr) {
    mca_mpool_udreg_hugepage_alloc_t *alloc;
    int rc;

    alloc = OBJ_NEW(mca_mpool_udreg_hugepage_alloc_t);
    alloc->size = size;

    rc = asprintf (&alloc->path, "%s/hugepage.openmpi.%d.%d", mpool->huge_page->path,
                   getpid (), mpool->huge_page->cnt++);
    if (0 > rc) {
        OBJ_RELEASE(alloc);
        return -1;
    }

    alloc->fd = open (alloc->path, O_RDWR | O_CREAT, 0600);
    if (-1 == alloc->fd) {
        OBJ_RELEASE(alloc);
        return -1;
    }

    if (0 != ftruncate (alloc->fd, size)) {
        close (alloc->fd);
        unlink (alloc->path);
        OBJ_RELEASE(alloc);
        return -1;
    }

    alloc->ptr = mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED,
                       alloc->fd, 0);
    if (NULL == alloc->ptr) {
        OBJ_RELEASE(alloc);
        return -1;
    }

    close (alloc->fd);
    unlink (alloc->path);

    alloc->huge_table = mpool->huge_page;

    opal_list_append (&mpool->huge_page->allocations, &alloc->super);

    *addr = alloc->ptr;
    *base_addr = alloc;

    return 0;
}


static void mca_mpool_udreg_free_huge (mca_mpool_udreg_hugepage_alloc_t *alloc) {
    opal_list_remove_item (&alloc->huge_table->allocations, &alloc->super);
    OBJ_RELEASE(alloc);
}

/**
  * allocate function
  */
void* mca_mpool_udreg_alloc(mca_mpool_base_module_t *mpool, size_t size,
                            size_t align, uint32_t flags, mca_mpool_base_registration_t **reg)
{
    mca_mpool_udreg_module_t *udreg_module = (mca_mpool_udreg_module_t *) mpool;
    void *base_addr, *addr;

    if(0 == align)
        align = mca_mpool_base_page_size;

#if OMPI_CUDA_SUPPORT
    /* CUDA cannot handle registering overlapping regions, so make
     * sure each region is page sized and page aligned. */
    align = mca_mpool_base_page_size;
    size = OPAL_ALIGN(size, mca_mpool_base_page_size, size_t);
#endif

    addr = base_addr = NULL;

    if (NULL != udreg_module->huge_page) {
        size = OPAL_ALIGN(size, udreg_module->huge_page->page_size, size_t);
        mca_mpool_udreg_alloc_huge (udreg_module, size, &addr, &base_addr);
    } else {
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
    }

    if (OMPI_SUCCESS != mca_mpool_udreg_register(mpool, addr, size, flags, reg)) {
        if (udreg_module->huge_page) {
            mca_mpool_udreg_free_huge ((mca_mpool_udreg_hugepage_alloc_t *) base_addr);
        } else {
            free(base_addr);
        }

        return NULL;
    }

    (*reg)->alloc_base = (unsigned char *) base_addr;

    return addr;
}

bool mca_mpool_udreg_evict (struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t *) mpool;
    udreg_return_t urc;

    urc = UDREG_Evict (mpool_udreg->udreg_handle);
    return (UDREG_RC_SUCCESS == urc);
}

/*
 * register memory
 */
int mca_mpool_udreg_register(mca_mpool_base_module_t *mpool, void *addr,
                             size_t size, uint32_t flags,
                             mca_mpool_base_registration_t **reg)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t *) mpool;
    mca_mpool_base_registration_t *udreg_reg;
    bool bypass_cache = !!(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS);
    udreg_entry_t *udreg_entry;
    udreg_return_t urc;

    if (false == bypass_cache) {
        /* Get a udreg entry for this region */
        while (UDREG_RC_SUCCESS !=
               (urc = UDREG_Register (mpool_udreg->udreg_handle, addr, size, &udreg_entry))) {
            /* try to remove one unused reg and retry */
            if (!mca_mpool_udreg_evict (mpool)) {
                *reg = NULL;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        udreg_reg = (mca_mpool_base_registration_t *) udreg_entry->device_data;
        udreg_reg->mpool_context = udreg_entry;
    } else {
        /* if cache bypass is requested don't use the udreg cache */
        while (NULL == (udreg_reg = mca_mpool_udreg_reg_func (addr, size, mpool))) {
            /* try to remove one unused reg and retry */
            if (!mca_mpool_udreg_evict (mpool)) {
                *reg = NULL;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
        udreg_reg->mpool_context = NULL;
    }

    udreg_reg->flags = flags;

    *reg = udreg_reg;
    (*reg)->ref_count++;

    return OMPI_SUCCESS;
}


/**
  * realloc function
  */
void* mca_mpool_udreg_realloc(mca_mpool_base_module_t *mpool, void *addr,
                              size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_base_registration_t *old_reg  = *reg;
    void *new_mem = mca_mpool_udreg_alloc(mpool, size, 0, old_reg->flags, reg);
    memcpy(new_mem, addr, old_reg->bound - old_reg->base + 1);
    mca_mpool_udreg_free(mpool, addr, old_reg);

    return new_mem;
}

/**
  * free function
  */
void mca_mpool_udreg_free(mca_mpool_base_module_t *mpool, void *addr,
                          mca_mpool_base_registration_t *registration)
{
    mca_mpool_udreg_module_t *udreg_module = (mca_mpool_udreg_module_t *) mpool;
    mca_mpool_udreg_deregister(mpool, registration);

    if (udreg_module->huge_page) {
        mca_mpool_udreg_free_huge ((mca_mpool_udreg_hugepage_alloc_t *) registration->alloc_base);
    } else {
        free (registration->alloc_base);
    }
}

int mca_mpool_udreg_find(struct mca_mpool_base_module_t *mpool, void *addr,
        size_t size, mca_mpool_base_registration_t **reg)
{
    *reg = NULL;
    return OMPI_ERR_NOT_FOUND;
}

int mca_mpool_udreg_deregister(struct mca_mpool_base_module_t *mpool,
                               mca_mpool_base_registration_t *reg)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t *) mpool;

    assert(reg->ref_count > 0);

    reg->ref_count--;

    if (0 == reg->ref_count && reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS) {
        mca_mpool_udreg_dereg_func (reg, mpool);
    } else if (!(reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS)) {
        UDREG_DecrRefcount (mpool_udreg->udreg_handle, reg->mpool_context);
    }

    return OMPI_SUCCESS;
}

void mca_mpool_udreg_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_udreg_module_t *mpool_udreg = (mca_mpool_udreg_module_t*)mpool;

    /* Statistic */
    if (true == mca_mpool_udreg_component.print_stats) {
        uint64_t hit = 0, miss = 0, evicted = 0;

        (void) UDREG_GetStat (mpool_udreg->udreg_handle,
                              UDREG_STAT_CACHE_HIT, &hit);

        (void) UDREG_GetStat (mpool_udreg->udreg_handle,
                              UDREG_STAT_CACHE_MISS, &miss);

        (void) UDREG_GetStat (mpool_udreg->udreg_handle,
                              UDREG_STAT_CACHE_EVICTED, &evicted);

        opal_output(0, "%s udreg: stats (hit/miss/evicted): %" PRIu64 "/%" PRIu64 "/%" PRIu64 "\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hit, miss, evicted);
    }

    UDREG_CacheRelease (mpool_udreg->udreg_handle);
    OBJ_DESTRUCT(&mpool_udreg->reg_list);
}

int mca_mpool_udreg_ft_event(int state) {
    return OMPI_SUCCESS;
}










