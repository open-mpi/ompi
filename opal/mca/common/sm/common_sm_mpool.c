/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2014 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "common_sm_mpool.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/common/sm/common_sm.h"
#include "opal/util/printf.h"
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include "opal/mca/hwloc/base/base.h"

static void sm_module_finalize(mca_mpool_base_module_t *module);

/*
 *  Returns base address of shared memory mapping.
 */
static void *mca_common_sm_mpool_base(mca_mpool_base_module_t *mpool);

/**
 *  Allocate block of shared memory.
 */
static void *mca_common_sm_mpool_alloc(mca_mpool_base_module_t *mpool, size_t size, size_t align,
                                       uint32_t flags);

/**
 * free function typedef
 */
static void mca_common_sm_mpool_free(mca_mpool_base_module_t *mpool, void *addr);

/*
 *  Initializes the mpool module.
 */
static void mca_common_sm_mpool_module_init(mca_common_sm_mpool_module_t *mpool)
{
    mpool->super.mpool_base = mca_common_sm_mpool_base;
    mpool->super.mpool_alloc = mca_common_sm_mpool_alloc;
    mpool->super.mpool_free = mca_common_sm_mpool_free;
    mpool->super.mpool_finalize = sm_module_finalize;
    mpool->super.flags = 0;

    mpool->sm_size = 0;
    mpool->sm_allocator = NULL;
    mpool->sm_mmap = NULL;
    mpool->sm_common_module = NULL;
    mpool->mem_node = -1;
}

mca_mpool_base_module_t *opal_btl_smcuda_common_sm_mpool_create(mca_common_sm_mpool_resources_t *resources)
{
    mca_common_sm_mpool_module_t *mpool_module;
    mca_allocator_base_component_t *allocator_component;

    /* Make a new mpool module */
    mpool_module = (mca_common_sm_mpool_module_t *) malloc(sizeof(*mpool_module));
    mca_common_sm_mpool_module_init(mpool_module);

    /* set sm_size */
    mpool_module->sm_size = resources->size;

    allocator_component = mca_allocator_component_lookup(resources->allocator);

    /* if specified allocator cannot be loaded - look for an alternative */
    if (NULL == allocator_component) {
        if (opal_list_get_size(&opal_allocator_base_framework.framework_components) == 0) {
            mca_base_component_list_item_t *item = (mca_base_component_list_item_t *)
                opal_list_get_first(&opal_allocator_base_framework.framework_components);
            allocator_component = (mca_allocator_base_component_t *) item->cli_component;
            opal_output(0,
                        "mca_common_sm_mpool_init: "
                        "unable to locate allocator: %s - using %s\n",
                        resources->allocator,
                        allocator_component->allocator_version.mca_component_name);
        } else {
            opal_output(0,
                        "mca_common_sm_mpool_init: "
                        "unable to locate allocator: %s\n",
                        resources->allocator);
            free(mpool_module);
            return NULL;
        }
    }

    mpool_module->mem_node = resources->mem_node;

    if (NULL
        == (mpool_module->sm_common_module = mca_common_sm_module_attach(
                &resources->bs_meta_buf, sizeof(mca_common_sm_module_t), 8))) {
        opal_output(0,
                    "mca_common_sm_mpool_init: "
                    "unable to create shared memory mapping (%s)",
                    resources->bs_meta_buf.seg_name);
        free(mpool_module);
        return NULL;
    }

    /* setup allocator */
    mpool_module->sm_allocator = allocator_component
                                     ->allocator_init(true, mca_common_sm_seg_alloc, NULL,
                                                      mpool_module->sm_common_module);
    if (NULL == mpool_module->sm_allocator) {
        opal_output(0, "mca_common_sm_mpool_init: unable to initialize allocator");
        free(mpool_module);
        return NULL;
    }

    return &mpool_module->super;
}

/*
 * base address of shared memory mapping
 */
static void *mca_common_sm_mpool_base(mca_mpool_base_module_t *mpool)
{
    mca_common_sm_mpool_module_t *sm_mpool = (mca_common_sm_mpool_module_t *) mpool;
    return (NULL != sm_mpool->sm_common_module) ? sm_mpool->sm_common_module->module_seg_addr
                                                : NULL;
}

/**
 * allocate function
 */
static void *mca_common_sm_mpool_alloc(mca_mpool_base_module_t *mpool, size_t size, size_t align,
                                       uint32_t flags)
{
    mca_common_sm_mpool_module_t *mpool_sm = (mca_common_sm_mpool_module_t *) mpool;
    opal_hwloc_base_memory_segment_t mseg;

    mseg.mbs_start_addr = mpool_sm->sm_allocator->alc_alloc(mpool_sm->sm_allocator, size, align);

    if (mpool_sm->mem_node >= 0) {
        mseg.mbs_len = size;
        opal_hwloc_base_membind(&mseg, 1, mpool_sm->mem_node);
    }

    return mseg.mbs_start_addr;
}

/**
 * free function
 */
void mca_common_sm_mpool_free(mca_mpool_base_module_t *mpool, void *addr)
{
    mca_common_sm_mpool_module_t *mpool_sm = (mca_common_sm_mpool_module_t *) mpool;
    mpool_sm->sm_allocator->alc_free(mpool_sm->sm_allocator, addr);
}

static void sm_module_finalize(mca_mpool_base_module_t *module)
{
    mca_common_sm_mpool_module_t *sm_module = (mca_common_sm_mpool_module_t *) module;

    if (NULL != sm_module->sm_common_module) {
        if (OPAL_SUCCESS == mca_common_sm_fini(sm_module->sm_common_module)) {
            unlink(sm_module->sm_common_module->shmem_ds.seg_name);
        }
        OBJ_RELEASE(sm_module->sm_common_module);
        sm_module->sm_common_module = NULL;
    }
}
