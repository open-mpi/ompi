/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2012-2016 Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Cisco Systems, Inc.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/constants.h"
#include "rcache_base_mem_cb.h"

/* two-level macro for stringifying a number */
#define STRINGIFYX(x) #x
#define STRINGIFY(x) STRINGIFYX(x)

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "opal/mca/rcache/base/static-components.h"

int mca_rcache_base_used_mem_hooks = 0;

/**
 * Memory Pool Registration
 */

static void mca_rcache_base_registration_constructor( mca_rcache_base_registration_t * reg )
{
    reg->rcache = NULL;
    reg->base = NULL;
    reg->bound = NULL;
    reg->ref_count = 0;
    reg->flags = 0;
}

OBJ_CLASS_INSTANCE(mca_rcache_base_registration_t, opal_free_list_item_t,
                   mca_rcache_base_registration_constructor, NULL);

#define TREE_ITEMS_MIN 2048
#define TREE_ITEMS_MAX 16384
#define TREE_ITEMS_INC 2048

/*
 * Global variables
 */
opal_list_t mca_rcache_base_modules = {{0}};
opal_free_list_t mca_rcache_base_vma_tree_items = {{{0}}};
bool mca_rcache_base_vma_tree_items_inited = false;
unsigned int mca_rcache_base_vma_tree_items_min = TREE_ITEMS_MIN;
int mca_rcache_base_vma_tree_items_max = TREE_ITEMS_MAX;
unsigned int mca_rcache_base_vma_tree_items_inc = TREE_ITEMS_INC;

OBJ_CLASS_INSTANCE(mca_rcache_base_selected_module_t, opal_list_item_t, NULL, NULL);

static int mca_rcache_base_close(void)
{
    opal_list_item_t *item;
    mca_rcache_base_selected_module_t *sm;

    /* Finalize all the rcache components and free their list items */

    while (NULL != (item = opal_list_remove_first(&mca_rcache_base_modules))) {
        sm = (mca_rcache_base_selected_module_t *) item;

        /* Blatently ignore the return code (what would we do to recover,
           anyway?  This component is going away, so errors don't matter
           anymore).  Note that it's legal for the module to have NULL for
           the finalize function. */

        if (NULL != sm->rcache_module->rcache_finalize) {
            sm->rcache_module->rcache_finalize(sm->rcache_module);
        }
        OBJ_RELEASE(sm);
    }

    /* deregister memory free callback */
    if (mca_rcache_base_used_mem_hooks) {
        opal_mem_hooks_unregister_release(mca_rcache_base_mem_cb);

        /* close the memory manager components.  Registered hooks can
           still be fired any time between now and the call to
           opal_mem_free_finalize(), and callbacks from the memory manager
           hooks to the bowels of the mem_free code can still occur any
           time between now and end of application (even post main()!) */
        (void) mca_base_framework_close (&opal_memory_base_framework);
    }

    OBJ_DESTRUCT(&mca_rcache_base_vma_tree_items);
    mca_rcache_base_vma_tree_items_inited = false;

    /* All done */
    /* Close all remaining available components */
    return mca_base_framework_components_close(&opal_rcache_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_rcache_base_open(mca_base_open_flag_t flags)
{
    /* Initialize the list so that in mca_rcache_base_close(), we can
       iterate over it (even if it's empty, as in the case of the opal_info-tool) */

    OBJ_CONSTRUCT(&mca_rcache_base_modules, opal_list_t);

    /* the free list is only initialized when a VMA tree is created */
    OBJ_CONSTRUCT(&mca_rcache_base_vma_tree_items, opal_free_list_t);

     /* Open up all available components */
    return mca_base_framework_components_open(&opal_rcache_base_framework, flags);
}

static int mca_rcache_base_register_mca_variables (mca_base_register_flag_t flags)
{

    mca_rcache_base_vma_tree_items_min = TREE_ITEMS_MIN;
    (void) mca_base_framework_var_register (&opal_rcache_base_framework, "vma_tree_items_min",
                                            "Minimum number of VMA tree items to allocate (default: "
                                            STRINGIFY(TREE_ITEMS_MIN) ")", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_base_vma_tree_items_min);

    mca_rcache_base_vma_tree_items_max = TREE_ITEMS_MAX;
    (void) mca_base_framework_var_register (&opal_rcache_base_framework, "vma_tree_items_max",
                                            "Maximum number of VMA tree items to allocate (default: "
                                            STRINGIFY(TREE_ITEMS_MAX) ", -1: unlimited)", MCA_BASE_VAR_TYPE_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_base_vma_tree_items_max);

    mca_rcache_base_vma_tree_items_inc = TREE_ITEMS_INC;
    (void) mca_base_framework_var_register (&opal_rcache_base_framework, "vma_tree_items_inc",
                                            "Number of VMA tree items to allocate at a time (default: "
                                            STRINGIFY(TREE_ITEMS_INC) ")", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_rcache_base_vma_tree_items_inc);

    return OPAL_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(opal, rcache, "OPAL Registration Cache",
                           mca_rcache_base_register_mca_variables,
                           mca_rcache_base_open, mca_rcache_base_close,
                           mca_rcache_base_static_components, 0);

