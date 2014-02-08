/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
static mca_memheap_base_module_t* mca_memheap_ptmalloc_component_init(memheap_context_t *,
                                                                      int *);

static int _basic_open(void);

mca_memheap_base_component_t mca_memheap_ptmalloc_component = {
    {
        MCA_MEMHEAP_BASE_VERSION_2_0_0,

        "ptmalloc", /* MCA component name */
        OSHMEM_MAJOR_VERSION,  /* MCA component major version */
        OSHMEM_MINOR_VERSION,  /* MCA component minor version */
        OSHMEM_RELEASE_VERSION,  /* MCA component release version */
        
        _basic_open,
        mca_memheap_ptmalloc_component_close,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    mca_memheap_ptmalloc_component_init
};

/* Open component */
static int _basic_open(void)
{
    return OSHMEM_SUCCESS;
}

/* Initialize component */
mca_memheap_base_module_t* mca_memheap_ptmalloc_component_init(memheap_context_t *context,
                                                               int *priority)
{
    int rc;

    *priority = memheap_ptmalloc.priority;
    rc = mca_memheap_ptmalloc_module_init(context);
    if (OSHMEM_SUCCESS != rc) {
        return NULL ;
    }

    return &(memheap_ptmalloc.super);
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
