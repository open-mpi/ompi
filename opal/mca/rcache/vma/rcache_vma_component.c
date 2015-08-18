/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/rcache/rcache.h"
#include "rcache_vma.h"

static int mca_rcache_vma_component_open(void);

static mca_rcache_base_module_t* mca_rcache_vma_component_init( void );

mca_rcache_vma_component_t mca_rcache_vma_component = {
    {
        .rcache_version = {
            MCA_RCACHE_BASE_VERSION_2_0_0,

            .mca_component_name = "vma",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),
            .mca_open_component = mca_rcache_vma_component_open,
        },
        .rcache_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .rcache_init = mca_rcache_vma_component_init,
    }
};


static int mca_rcache_vma_component_open(void)
{
    return OPAL_SUCCESS;
}

static mca_rcache_base_module_t* mca_rcache_vma_component_init(void) {
    mca_rcache_vma_module_t* rcache;

    rcache = (mca_rcache_vma_module_t*) malloc(sizeof(mca_rcache_vma_module_t));
    mca_rcache_vma_module_init(rcache);

    return &rcache->base;
}
