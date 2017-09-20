/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
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
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "rcache_vma.h"

static int mca_rcache_vma_component_open (void);
static int mca_rcache_vma_component_close (void);
static int mca_rcache_vma_component_register (void);

static mca_rcache_base_module_t* mca_rcache_vma_component_init( void );

/* two-level macro for stringifying a number */
#define STRINGIFYX(x) #x
#define STRINGIFY(x) STRINGIFYX(x)

#define TREE_ITEMS_MIN 2048
#define TREE_ITEMS_MAX 16384
#define TREE_ITEMS_INC 2048

opal_free_list_t mca_rcache_vma_tree_items = {{{0}}};
bool mca_rcache_vma_tree_items_inited = false;
unsigned int mca_rcache_vma_tree_items_min = TREE_ITEMS_MIN;
int mca_rcache_vma_tree_items_max = TREE_ITEMS_MAX;
unsigned int mca_rcache_vma_tree_items_inc = TREE_ITEMS_INC;

mca_rcache_vma_component_t mca_rcache_vma_component = {
    {
        .rcache_version = {
            MCA_RCACHE_BASE_VERSION_2_0_0,

            .mca_component_name = "vma",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),
            .mca_open_component = mca_rcache_vma_component_open,
            .mca_close_component = mca_rcache_vma_component_close,
            .mca_register_component_params = mca_rcache_vma_component_register,
        },
        .rcache_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .rcache_init = mca_rcache_vma_component_init,
    }
};

static int mca_rcache_vma_component_register (void)
{
    mca_rcache_vma_tree_items_min = TREE_ITEMS_MIN;
    (void) mca_base_component_var_register (&mca_rcache_vma_component.super.rcache_version, "vma_tree_items_min",
                                            "Minimum number of VMA tree items to allocate (default: "
                                            STRINGIFY(TREE_ITEMS_MIN) ")", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_vma_tree_items_min);

    mca_rcache_vma_tree_items_max = TREE_ITEMS_MAX;
    (void) mca_base_component_var_register (&mca_rcache_vma_component.super.rcache_version, "vma_tree_items_max",
                                            "Maximum number of VMA tree items to allocate (default: "
                                            STRINGIFY(TREE_ITEMS_MAX) ", -1: unlimited)", MCA_BASE_VAR_TYPE_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_vma_tree_items_max);

    mca_rcache_vma_tree_items_inc = TREE_ITEMS_INC;
    (void) mca_base_component_var_register (&mca_rcache_vma_component.super.rcache_version, "vma_tree_items_inc",
                                            "Number of VMA tree items to allocate at a time (default: "
                                            STRINGIFY(TREE_ITEMS_INC) ")", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_vma_tree_items_inc);

    return OPAL_SUCCESS;
}

static int mca_rcache_vma_component_open (void)
{
    /* the free list is only initialized when a VMA tree is created */
    OBJ_CONSTRUCT(&mca_rcache_vma_tree_items, opal_free_list_t);
    return OPAL_SUCCESS;
}

static int mca_rcache_vma_component_close (void)
{
    OBJ_DESTRUCT(&mca_rcache_vma_tree_items);
    mca_rcache_vma_tree_items_inited = false;
    return OPAL_SUCCESS;
}

static mca_rcache_base_module_t* mca_rcache_vma_component_init(void) {
    mca_rcache_vma_module_t* rcache;

    rcache = (mca_rcache_vma_module_t*) malloc(sizeof(mca_rcache_vma_module_t));
    mca_rcache_vma_module_init(rcache);

    return &rcache->base;
}
