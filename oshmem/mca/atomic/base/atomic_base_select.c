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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oshmem/constants.h"

#include "opal/class/opal_list.h"
#include "oshmem/util/oshmem_util.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
mca_atomic_base_module_t mca_atomic;

/*
 * Local types
 */
struct avail_com_t {
    opal_list_item_t super;

    int ac_priority;
    mca_atomic_base_module_t *ac_module;
};
typedef struct avail_com_t avail_com_t;

/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t * components);
static int check_one_component(const mca_base_component_t * component,
                               mca_atomic_base_module_1_0_0_t ** module);

static int query(const mca_base_component_t * component,
                 int *priority,
                 mca_atomic_base_module_1_0_0_t ** module);

static int query_1_0_0(const mca_atomic_base_component_1_0_0_t * atomic_component,
                       int *priority,
                       mca_atomic_base_module_1_0_0_t ** module);

/*
 * Stuff for the OBJ interface
 */
static OBJ_CLASS_INSTANCE(avail_com_t, opal_list_item_t, NULL, NULL);

/*
 * This function is called at the initialization.  
 * It is used to select which atomic component will be
 * active for a given group.
 */
int mca_atomic_base_select(void)
{
    opal_list_t *selectable;
    opal_list_item_t *item;

    /* Announce */
    ATOMIC_VERBOSE(10,
                   "atomic:base:atomic_select: Checking all available modules");
    selectable = check_components(&oshmem_atomic_base_framework.framework_components);

    /* Upon return from the above, the modules list will contain the
     list of modules that returned (priority >= 0).  If we have no
     atomic modules available, then print error and return. */
    if (NULL == selectable) {
        /* There's no modules available */
        return OSHMEM_ERROR;
    }

    /* do the selection loop */
    for (item = opal_list_remove_first(selectable); NULL != item; item =
            opal_list_remove_first(selectable)) {
        avail_com_t *avail = (avail_com_t *) item;

        /* Set module having the highest priority */
        memcpy(&mca_atomic, avail->ac_module, sizeof(mca_atomic));

        OBJ_RELEASE(avail->ac_module);
        OBJ_RELEASE(avail);
        /* check correctness */
        if (!(mca_atomic.atomic_fadd) || !(mca_atomic.atomic_cswap)) {
            return OSHMEM_ERR_NOT_FOUND;
        }
    }

    /* Done with the list from the check_components() call so release it. */
    OBJ_RELEASE(selectable);

    return OSHMEM_SUCCESS;
}

static int avail_com_compare (opal_list_item_t **a,
                               opal_list_item_t **b)
{
    avail_com_t *acom = (avail_com_t *) *a;
    avail_com_t *bcom = (avail_com_t *) *b;

    if (acom->ac_priority > bcom->ac_priority) {
        return 1;
    } else if (acom->ac_priority < bcom->ac_priority) {
        return -1;
    }

    return 0;
}

/*
 * For each module in the list, check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of modules to be
 * only those who returned that they want to run, and put them in
 * priority order.
 */
static opal_list_t *check_components(opal_list_t *components)
{
    int priority;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    mca_atomic_base_module_1_0_0_t *module;
    opal_list_t *selectable;
    avail_com_t *avail;

    /* Make a list of the components that query successfully */
    selectable = OBJ_NEW(opal_list_t);

    /* Scan through the list of components */
    OPAL_LIST_FOREACH(cli, &oshmem_atomic_base_framework.framework_components, mca_base_component_list_item_t) {
        component = cli->cli_component;

        priority = check_one_component(component, &module);
        if (priority >= 0) {
            /* We have a component that indicated that it wants to run
               by giving us a module */
            avail = OBJ_NEW(avail_com_t);
            avail->ac_priority = priority;
            avail->ac_module = module;

            opal_list_append(selectable, &avail->super);
        }
    }

    /* If we didn't find any available components, return an error */
    if (0 == opal_list_get_size(selectable)) {
        OBJ_RELEASE(selectable);
        return NULL;
    }

    /* Put this list in priority order */
    opal_list_sort(selectable, avail_com_compare);

    /* All done */
    return selectable;
}

/*
 * Check a single component
 */
static int check_one_component(const mca_base_component_t *component,
                               mca_atomic_base_module_1_0_0_t **module)
{
    int err;
    int priority = -1;

    err = query(component, &priority, module);

    if (OSHMEM_SUCCESS == err) {
        priority = (priority < 100) ? priority : 100;
        ATOMIC_VERBOSE(10,
                       "atomic:base:atomic_select: component available: %s, priority: %d",
                       component->mca_component_name, priority);

    } else {
        priority = -1;
        ATOMIC_VERBOSE(10,
                       "atomic:base:atomic_select: component not available: %s",
                       component->mca_component_name);
    }

    return priority;
}

/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a atomic module, query it, and return the right
 * module struct
 */
static int query(const mca_base_component_t *component,
                 int *priority,
                 mca_atomic_base_module_1_0_0_t **module)
{
    *module = NULL;
    if (1 == component->mca_type_major_version
            && 0 == component->mca_type_minor_version
            && 0 == component->mca_type_release_version) {
        const mca_atomic_base_component_1_0_0_t *atomic100 =
                (mca_atomic_base_component_1_0_0_t *) component;

        return query_1_0_0(atomic100, priority, module);
    }

    /* Unknown atomic API version -- return error */

    return OSHMEM_ERROR;
}

static int query_1_0_0(const mca_atomic_base_component_1_0_0_t *component,
                       int *priority,
                       mca_atomic_base_module_1_0_0_t **module)
{
    mca_atomic_base_module_1_0_0_t *ret;

    /* There's currently no need for conversion */

    ret = component->atomic_query(priority);
    if (NULL != ret) {
        *module = ret;
        return OSHMEM_SUCCESS;
    }

    return OSHMEM_ERROR;
}
