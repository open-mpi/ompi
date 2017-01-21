/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"
#include "opal/util/output.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/memheap/ptmalloc/memheap_ptmalloc.h"
#include "memheap_ptmalloc_component.h"

static int mca_memheap_ptmalloc_component_close(void);
static int mca_memheap_ptmalloc_component_query(mca_base_module_t **module, 
                                                int *priority);

static int _basic_open(void);

mca_memheap_base_component_t mca_memheap_ptmalloc_component = {
    .memheap_version = {
        MCA_MEMHEAP_BASE_VERSION_2_0_0,

        .mca_component_name= "ptmalloc",
        MCA_BASE_MAKE_VERSION(component, OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION,
                              OSHMEM_RELEASE_VERSION),

        .mca_open_component = _basic_open,
        .mca_close_component = mca_memheap_ptmalloc_component_close,
        .mca_query_component = mca_memheap_ptmalloc_component_query,
    },
    .memheap_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .memheap_init = mca_memheap_ptmalloc_module_init,
};

/* Open component */
static int _basic_open(void)
{
    return OSHMEM_SUCCESS;
}

/* query component */
static int
mca_memheap_ptmalloc_component_query(mca_base_module_t **module, int *priority)
{
    *priority = memheap_ptmalloc.priority;
    *module = (mca_base_module_t *)&memheap_ptmalloc.super;
    return OSHMEM_SUCCESS;
}

/*
 * This function is automaticaly called from mca_base_components_close.
 * It releases the component's allocated memory.
 */
int mca_memheap_ptmalloc_component_close()
{
    mca_memheap_ptmalloc_finalize();
    return OSHMEM_SUCCESS;
}
