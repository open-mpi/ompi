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
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "opal_config.h"
#include "opal/align.h"
#include "rcache_udreg.h"
#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "opal/mca/rcache/base/base.h"
#include "opal/runtime/opal_params.h"
#include "opal/include/opal_stdint.h"
#include "opal/util/sys_limits.h"

#include <fcntl.h>

#include <udreg_pub.h>

#include <sys/mman.h>


static int mca_rcache_udreg_register (mca_rcache_base_module_t* rcache, void *addr,
                                      size_t size, uint32_t flags, int32_t access_flags,
                                      mca_rcache_base_registration_t **reg);
static int mca_rcache_udreg_deregister (mca_rcache_base_module_t *rcache,
                                        mca_rcache_base_registration_t *reg);
static int mca_rcache_udreg_find (mca_rcache_base_module_t* rcache, void* addr,
                                  size_t size, mca_rcache_base_registration_t **reg);
static void mca_rcache_udreg_finalize (mca_rcache_base_module_t *rcache);
static bool mca_rcache_udreg_evict (mca_rcache_base_module_t *rcache);

static void *mca_rcache_udreg_reg_func (void *addr, uint64_t len, void *reg_context);
static uint32_t mca_rcache_udreg_dereg_func (void *device_data, void *dreg_context);


/*
 *  Initializes the rcache module.
 */
int mca_rcache_udreg_module_init (mca_rcache_udreg_module_t *rcache)
{
    struct udreg_cache_attr cache_attr;
    int urc;

    rcache->super.rcache_component = &mca_rcache_udreg_component.super;
    rcache->super.rcache_register = mca_rcache_udreg_register;
    rcache->super.rcache_find = mca_rcache_udreg_find;
    rcache->super.rcache_deregister = mca_rcache_udreg_deregister;
    /* This module relies on udreg for notification of memory release */
    rcache->super.rcache_invalidate_range = NULL;
    rcache->super.rcache_finalize = mca_rcache_udreg_finalize;

    cache_attr.modes = 0;

    /* Create udreg cache */
    if (rcache->resources.use_kernel_cache) {
        cache_attr.modes |= UDREG_CC_MODE_USE_KERNEL_CACHE;
    }

    if (rcache->resources.use_evict_w_unreg) {
        cache_attr.modes |= UDREG_CC_MODE_USE_EVICT_W_UNREG;
    }

    if (mca_rcache_udreg_component.leave_pinned) {
        cache_attr.modes |= UDREG_CC_MODE_USE_LAZY_DEREG;
    }

    OBJ_CONSTRUCT(&rcache->lock, opal_mutex_t);

    strncpy (cache_attr.cache_name, rcache->resources.base.cache_name, UDREG_MAX_CACHENAME_LEN);
    cache_attr.max_entries         = rcache->resources.max_entries;
    cache_attr.debug_mode          = 0;
    cache_attr.debug_rank          = 0;
    cache_attr.reg_context         = rcache;
    cache_attr.dreg_context        = rcache;
    cache_attr.destructor_context  = rcache;
    cache_attr.device_reg_func     = mca_rcache_udreg_reg_func;
    cache_attr.device_dereg_func   = mca_rcache_udreg_dereg_func;
    cache_attr.destructor_callback = NULL;

    opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                         "rcache/udreg: creating udreg cache with name %s", cache_attr.cache_name);

    /* attempt to create the udreg cache. this will fail if one already exists */
    (void) UDREG_CacheCreate (&cache_attr);

    urc = UDREG_CacheAccess (rcache->resources.base.cache_name, (udreg_cache_handle_t *) &rcache->udreg_handle);
    if (UDREG_RC_SUCCESS != urc) {
        opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_rcache_base_framework.framework_output,
                             "rcache/udreg: call to UDREG_CacheAccess failed with rc: %d", urc);
        return OPAL_ERROR;
    }

    OBJ_CONSTRUCT(&rcache->reg_list, opal_free_list_t);
    opal_free_list_init (&rcache->reg_list, rcache->resources.base.sizeof_reg,
                         opal_cache_line_size, OBJ_CLASS(mca_rcache_base_registration_t),
                         0, opal_cache_line_size, 0, -1, 32, NULL, 0,
                         NULL, NULL, NULL);

    return OPAL_SUCCESS;
}

/* udreg callback functions */
static void *mca_rcache_udreg_reg_func (void *addr, uint64_t size, void *reg_context)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t *) reg_context;
    unsigned int page_size = opal_getpagesize ();
    mca_rcache_base_registration_t *udreg_reg;
    opal_free_list_item_t *item;
    int rc;

    item = opal_free_list_get (&rcache_udreg->reg_list);
    if (NULL == item) {
        return NULL;
    }

    udreg_reg = (mca_rcache_base_registration_t *) item;

    udreg_reg->rcache = reg_context;
    udreg_reg->base  = OPAL_DOWN_ALIGN_PTR(addr, page_size, unsigned char *);
    udreg_reg->bound = OPAL_ALIGN_PTR((intptr_t) addr + size, page_size, unsigned char *) - 1;
    udreg_reg->ref_count = 0;

    addr = (void *) udreg_reg->base;
    size = (uint64_t) (udreg_reg->bound - udreg_reg->base + 1);

    /* pull the flags and access flags out of the rcache module */
    udreg_reg->access_flags = rcache_udreg->requested_access_flags;
    udreg_reg->flags = rcache_udreg->requested_flags;

    opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                         "rcache/udreg: calling underlying register function for address range {%p, %p}",
                         addr, (void *)((intptr_t) addr + size));
    rc = rcache_udreg->resources.base.register_mem (rcache_udreg->resources.base.reg_data, udreg_reg->base, size,
                                                    udreg_reg);
    if (OPAL_SUCCESS != rc) {
        opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_rcache_base_framework.framework_output,
                             "rcache/udreg: could not register memory. rc: %d", rc);
        opal_free_list_return (&rcache_udreg->reg_list, item);
        /* NTH: this is the only way to get UDReg_Register to recognize a failure */
        udreg_reg = UDREG_DEVICE_REG_FAILED;
    }

    return udreg_reg;
}

static uint32_t mca_rcache_udreg_dereg_func (void *device_data, void *dreg_context)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t *) dreg_context;
    mca_rcache_base_registration_t *udreg_reg = (mca_rcache_base_registration_t *) device_data;
    int rc;

    assert (udreg_reg->ref_count == 0);

    rc = rcache_udreg->resources.base.deregister_mem (rcache_udreg->resources.base.reg_data, udreg_reg);
    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_free_list_return (&rcache_udreg->reg_list,
                               (opal_free_list_item_t *) udreg_reg);
    }
    /* might be worth printing out a warning if an error occurs here */

    return 0;
}

static bool mca_rcache_udreg_evict (mca_rcache_base_module_t *rcache)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t *) rcache;
    udreg_return_t urc;

    urc = UDREG_Evict (rcache_udreg->udreg_handle);
    return (UDREG_RC_SUCCESS == urc);
}

/*
 * register memory
 */
static int mca_rcache_udreg_register(mca_rcache_base_module_t *rcache, void *addr,
                                    size_t size, uint32_t flags, int32_t access_flags,
                                    mca_rcache_base_registration_t **reg)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t *) rcache;
    mca_rcache_base_registration_t *udreg_reg, *old_reg;
    bool bypass_cache = !!(flags & MCA_RCACHE_FLAGS_CACHE_BYPASS);
    const unsigned int page_size = opal_getpagesize ();
    unsigned char *base, *bound;
    udreg_entry_t *udreg_entry = NULL;

    *reg = NULL;

    OPAL_THREAD_LOCK(&rcache_udreg->lock);

    /* we hold the lock so no other thread can modify these flags until the registration is complete */
    rcache_udreg->requested_access_flags = access_flags;
    rcache_udreg->requested_flags = flags;

    base = OPAL_DOWN_ALIGN_PTR(addr, page_size, unsigned char *);
    bound = OPAL_ALIGN_PTR((intptr_t) addr + size, page_size, unsigned char *) - 1;

    addr = base;
    size = (size_t) (uintptr_t) (bound - base) + 1;

    if (false == bypass_cache) {
        /* Get a udreg entry for this region */
        do {
            opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                                 "rcache/udreg: XXX registering region {%p, %p} with udreg", addr, (void *)((intptr_t) addr + size));
            while (UDREG_RC_SUCCESS != UDREG_Register (rcache_udreg->udreg_handle, addr, size, &udreg_entry)) {
                /* try to remove one unused reg and retry */
                opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                                     "calling evict!");
                if (!mca_rcache_udreg_evict (rcache)) {
                    opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                                         "rcache/udreg: could not register memory with udreg");
                    OPAL_THREAD_UNLOCK(&rcache_udreg->lock);
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
            }

            udreg_reg = (mca_rcache_base_registration_t *) udreg_entry->device_data;
            if (NULL != udreg_reg && (udreg_reg->access_flags & access_flags) == access_flags) {
                /* sufficient access */
                break;
            }

            old_reg = udreg_reg;

            if (old_reg) {
                /* to not confuse udreg make sure the new registration covers the same address
                 * range as the old one. */
                addr = old_reg->base;
                size = (size_t)((intptr_t) old_reg->bound - (intptr_t) old_reg->base);

                /* make the new access flags more permissive */
                access_flags |= old_reg->access_flags;

                if (!old_reg->ref_count) {
                    /* deregister the region before attempting to re-register */
                    mca_rcache_udreg_dereg_func (old_reg, rcache);
                    udreg_entry->device_data = NULL;
                    old_reg = NULL;
                } else {
                    /* ensure that mca_rcache_udreg_deregister does not call into udreg since
                     * we are forcefully evicting the registration here */
                    old_reg->flags |= MCA_RCACHE_FLAGS_CACHE_BYPASS | MCA_RCACHE_FLAGS_INVALID;
                }
            }

            rcache_udreg->requested_access_flags = access_flags;

            /* get a new registration */
            while (UDREG_DEVICE_REG_FAILED == (udreg_reg = mca_rcache_udreg_reg_func (addr, size, rcache))) {
                if (!mca_rcache_udreg_evict (rcache)) {
                    opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                                         "rcache/udreg: could not register memory with udreg");
                    OPAL_THREAD_UNLOCK(&rcache_udreg->lock);
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
            }

            /* update the device data with the new registration */
            udreg_entry->device_data = udreg_reg;
        } while (0);
    } else {
        /* if cache bypass is requested don't use the udreg cache */
        while (UDREG_DEVICE_REG_FAILED == (udreg_reg = mca_rcache_udreg_reg_func (addr, size, rcache))) {
            /* try to remove one unused reg and retry */
            if (!mca_rcache_udreg_evict (rcache)) {
                opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_rcache_base_framework.framework_output,
                                     "rcache/udreg: could not register memory");
                OPAL_THREAD_UNLOCK(&rcache_udreg->lock);
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    OPAL_THREAD_UNLOCK(&rcache_udreg->lock);

    *reg = udreg_reg;
    (void) OPAL_THREAD_ADD_FETCH32(&udreg_reg->ref_count, 1);
    udreg_reg->rcache_context = udreg_entry;

    return OPAL_SUCCESS;
}

static int mca_rcache_udreg_find (mca_rcache_base_module_t *rcache, void *addr,
                                 size_t size, mca_rcache_base_registration_t **reg)
{
    *reg = NULL;
    return OPAL_ERR_NOT_FOUND;
}

static int mca_rcache_udreg_deregister(mca_rcache_base_module_t *rcache,
                                      mca_rcache_base_registration_t *reg)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t *) rcache;
    int32_t ref_count = OPAL_THREAD_ADD_FETCH32 (&reg->ref_count, -1);

    assert(ref_count >= 0);

    if (!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
        OPAL_THREAD_LOCK(&rcache_udreg->lock);
        UDREG_DecrRefcount (rcache_udreg->udreg_handle, reg->rcache_context);
        OPAL_THREAD_UNLOCK(&rcache_udreg->lock);
    } else if (!ref_count) {
        mca_rcache_udreg_dereg_func (reg, rcache);
    }

    return OPAL_SUCCESS;
}

static void mca_rcache_udreg_finalize (mca_rcache_base_module_t *rcache)
{
    mca_rcache_udreg_module_t *rcache_udreg = (mca_rcache_udreg_module_t*)rcache;

    /* Statistic */
    if (true == mca_rcache_udreg_component.print_stats) {
        uint64_t hit = 0, miss = 0, evicted = 0;

        (void) UDREG_GetStat (rcache_udreg->udreg_handle,
                              UDREG_STAT_CACHE_HIT, &hit);

        (void) UDREG_GetStat (rcache_udreg->udreg_handle,
                              UDREG_STAT_CACHE_MISS, &miss);

        (void) UDREG_GetStat (rcache_udreg->udreg_handle,
                              UDREG_STAT_CACHE_EVICTED, &evicted);

        opal_output(0, "%s udreg: stats (hit/miss/evicted): %" PRIu64 "/%" PRIu64 "/%" PRIu64 "\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), hit, miss, evicted);
    }

    UDREG_CacheRelease (rcache_udreg->udreg_handle);
    OBJ_DESTRUCT(&rcache_udreg->reg_list);
    OBJ_DESTRUCT(&rcache_udreg->lock);
}
