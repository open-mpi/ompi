/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2014 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <string.h>
#include "opal/mca/mpool/sm/mpool_sm.h"
#include "opal/mca/common/sm/common_sm.h"
#include "opal/mca/common/cuda/common_cuda.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/mca/hwloc/base/base.h"

static void sm_module_finalize(mca_mpool_base_module_t* module);

/*
 *  Initializes the mpool module.
 */
void mca_mpool_sm_module_init(mca_mpool_sm_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_sm_component.super;
    mpool->super.mpool_base = mca_mpool_sm_base;
    mpool->super.mpool_alloc = mca_mpool_sm_alloc;
    mpool->super.mpool_realloc = mca_mpool_sm_realloc;
    mpool->super.mpool_free = mca_mpool_sm_free;
    mpool->super.mpool_find = NULL;
    mpool->super.mpool_register = NULL;
    mpool->super.mpool_deregister = NULL;
    mpool->super.mpool_release_memory = NULL;
    mpool->super.mpool_finalize = sm_module_finalize;
    mpool->super.mpool_ft_event = NULL;
    mpool->super.flags = 0;

    mpool->sm_size = 0;
    mpool->sm_allocator = NULL;
    mpool->sm_mmap = NULL;
    mpool->sm_common_module = NULL;
    mpool->mem_node = -1;
}

/*
 * base address of shared memory mapping
 */
void* mca_mpool_sm_base(mca_mpool_base_module_t* mpool)
{
    mca_mpool_sm_module_t *sm_mpool = (mca_mpool_sm_module_t*) mpool;
    return (NULL != sm_mpool->sm_common_module) ?
        sm_mpool->sm_common_module->module_seg_addr : NULL;
}

/**
  * allocate function
  */
void* mca_mpool_sm_alloc(
    mca_mpool_base_module_t* mpool,
    size_t size,
    size_t align,
    uint32_t flags,
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool;
    opal_hwloc_base_memory_segment_t mseg;

    mseg.mbs_start_addr =
        mpool_sm->sm_allocator->alc_alloc(mpool_sm->sm_allocator, size, align, registration);

    if(mpool_sm->mem_node >= 0) {
        mseg.mbs_len = size;
        opal_hwloc_base_membind(&mseg, 1, mpool_sm->mem_node);
    }

    return mseg.mbs_start_addr;
}

/**
  * realloc function
  */
void* mca_mpool_sm_realloc(
    mca_mpool_base_module_t* mpool,
    void* addr,
    size_t size,
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool;
    opal_hwloc_base_memory_segment_t mseg;

    mseg.mbs_start_addr =
        mpool_sm->sm_allocator->alc_realloc(mpool_sm->sm_allocator, addr, size,
                                            registration);
    if(mpool_sm->mem_node >= 0) {
        mseg.mbs_len = size;
        opal_hwloc_base_membind(&mseg, 1, mpool_sm->mem_node);
    }

    return mseg.mbs_start_addr;
}

/**
  * free function
  */
void mca_mpool_sm_free(mca_mpool_base_module_t* mpool, void * addr,
                       mca_mpool_base_registration_t* registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool;
    mpool_sm->sm_allocator->alc_free(mpool_sm->sm_allocator, addr);
}

static void sm_module_finalize(mca_mpool_base_module_t* module)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t*) module;

    if (NULL != sm_module->sm_common_module) {
        if (OPAL_SUCCESS ==
            mca_common_sm_fini(sm_module->sm_common_module)) {
            unlink(sm_module->sm_common_module->shmem_ds.seg_name);
        }
        OBJ_RELEASE(sm_module->sm_common_module);
        sm_module->sm_common_module = NULL;
    }
}
