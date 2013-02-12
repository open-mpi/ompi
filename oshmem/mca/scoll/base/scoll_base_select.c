/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
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
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "oshmem/proc/proc.h"
#include "oshmem/runtime/runtime.h"


/*
 * Local types
 */
struct avail_coll_t {
    opal_list_item_t super;

    int ac_priority;
    mca_scoll_base_module_1_0_0_t *ac_module;
};
typedef struct avail_coll_t avail_coll_t;


/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t * components,
                                     oshmem_group_t * group);
static int check_one_component(oshmem_group_t * group,
                               const mca_base_component_t * component,
                               mca_scoll_base_module_1_0_0_t ** module);

static int query(const mca_base_component_t * component,
                 oshmem_group_t * group, int *priority,
                 mca_scoll_base_module_1_0_0_t ** module);

static int query_1_0_0(const mca_scoll_base_component_1_0_0_t *
                       scoll_component, oshmem_group_t * group,
                       int *priority,
                       mca_scoll_base_module_1_0_0_t ** module);

static int scoll_null_barrier(struct oshmem_group_t *group, long *pSync, int alg)
{
    if (oshmem_proc_group_is_member(group)) {
        SCOLL_ERROR("internal error");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static int scoll_null_broadcast(struct oshmem_group_t *group, int PE_root, void *target, const void *source, size_t nlong, long *pSync, int alg)
{
    if (oshmem_proc_group_is_member(group)) {
        SCOLL_ERROR("internal error");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static int scoll_null_collect(struct oshmem_group_t *group, void *target, const void *source, size_t nlong, long *pSync, bool nlong_type, int alg)
{
    if (oshmem_proc_group_is_member(group)) {
        SCOLL_ERROR("internal error");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static int scoll_null_reduce(struct oshmem_group_t *group, struct oshmem_op_t *op, void *target, const void *source, size_t nlong, long *pSync, void *pWrk, int alg)
{
    if (oshmem_proc_group_is_member(group)) {
        SCOLL_ERROR("internal error");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

/*
 * Stuff for the OBJ interface
 */
static OBJ_CLASS_INSTANCE(avail_coll_t, opal_list_item_t, NULL, NULL);


#define COPY(module, group, func)                                        \
    do {                                                                \
        if (NULL != module->scoll_ ## func) {                            \
            if (NULL != group->g_scoll.scoll_ ## func ## _module) {        \
                OBJ_RELEASE(group->g_scoll.scoll_ ## func ## _module);     \
            }                                                           \
            group->g_scoll.scoll_ ## func = module->scoll_ ## func;         \
            group->g_scoll.scoll_ ## func ## _module = module;             \
            OBJ_RETAIN(module);                                         \
        }                                                               \
    } while (0)

#define CLOSE(group, func)                                       \
    do {                                                        \
            if (NULL != group->g_scoll.scoll_ ## func ## _module) {    \
            OBJ_RELEASE(group->g_scoll.scoll_ ## func ## _module); \
            group->g_scoll.scoll_## func = NULL;                   \
            group->g_scoll.scoll_## func ## _module = NULL;        \
        }                                                       \
    } while (0)

int mca_scoll_base_group_unselect(struct oshmem_group_t * group)
{
    /* 
     * scoll close() is called before group destructors, so
     * do close group collectives if scoll modules are no longer
     * valid
     *
     * there is a memory leak here, because not doing close means
     * that we leaving object with dangling ref counts 
     */
    SCOLL_VERBOSE(10,"scoll:base:group_unselect: group: %d",
            group->id);
    if (mca_scoll_base_components_opened_valid ||
            mca_scoll_base_components_available_valid ){
        CLOSE(group, reduce);
        CLOSE(group, barrier);
        CLOSE(group, broadcast);
        CLOSE(group, collect);
    }
    /* All done */
    return OSHMEM_SUCCESS;
}
/*
 * This function is called at the initialization time of every
 * group.  It is used to select which coll component will be
 * active for a given group.
 */
int mca_scoll_base_select(struct oshmem_group_t *group)
{
    opal_list_t *selectable;
    opal_list_item_t *item;
    int ret;
    /* Announce */
    SCOLL_VERBOSE(10,"scoll:base:group_select: new group: %d",
                        group->id);
    mca_scoll_base_group_unselect(group);
    memset(&group->g_scoll, 0, sizeof(mca_scoll_base_group_scoll_t));
    if (!oshmem_proc_group_is_member(group)) {
        group->g_scoll.scoll_barrier = scoll_null_barrier;
        group->g_scoll.scoll_broadcast = scoll_null_broadcast;
        group->g_scoll.scoll_reduce = scoll_null_reduce;
        group->g_scoll.scoll_collect = scoll_null_collect;
        return OSHMEM_SUCCESS;
    }
    SCOLL_VERBOSE(10,"scoll:base:group_select: Checking all available modules");
    selectable = check_components(&mca_scoll_base_components_available, group);

    /* Upon return from the above, the modules list will contain the
       list of modules that returned (priority >= 0).  If we have no
       collective modules available, then print error and return. */
    if (NULL == selectable) {
        /* There's no modules available */
        return OSHMEM_ERROR;
    }

    /* do the selection loop */
    for (item = opal_list_remove_first(selectable);
         NULL != item; item = opal_list_remove_first(selectable)) 
    {
        avail_coll_t *avail = (avail_coll_t *)item;
        ret = avail->ac_module->scoll_module_enable(avail->ac_module, group);
        if (OSHMEM_SUCCESS != ret) {
            mca_scoll_base_group_unselect(group);
        }
        else {
            COPY(avail->ac_module, group, broadcast);
            COPY(avail->ac_module, group, collect);
            COPY(avail->ac_module, group, reduce);
            COPY(avail->ac_module, group, barrier);
        }
        OBJ_RELEASE(avail->ac_module);
        OBJ_RELEASE(avail);
    }


    /* Done with the list from the check_components() call so release it. */
    OBJ_RELEASE(selectable);
    if ((NULL == group->g_scoll.scoll_barrier) ||
            (NULL == group->g_scoll.scoll_broadcast) ||
            (NULL == group->g_scoll.scoll_collect) ||
            (NULL == group->g_scoll.scoll_reduce)) {
        mca_scoll_base_group_unselect(group);
        return OSHMEM_ERR_NOT_FOUND;
    }

    return OSHMEM_SUCCESS;
}


/*
 * For each module in the list, check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of modules to be
 * only those who returned that they want to run, and put them in
 * priority order.
 */
static opal_list_t *check_components(opal_list_t * components,
                                     oshmem_group_t * group)
{
    int priority;
    const mca_base_component_t *component;
    opal_list_item_t *item, *item2;
    mca_scoll_base_module_1_0_0_t *module;
    opal_list_t *selectable;
    avail_coll_t *avail, *avail2;

    /* Make a list of the components that query successfully */
    selectable = OBJ_NEW(opal_list_t);

    /* Scan through the list of components.  This nested loop is
       O(N^2), but we should never have too many components, so this
       *hopefully* shouldn't matter... */

    for (item = opal_list_get_first(components);
         ((item != opal_list_get_end(components)) && (item != NULL));
         item = opal_list_get_next(item)) {
        component = ((mca_base_component_priority_list_item_t *)
                     item)->super.cli_component;

        priority = check_one_component(group, component, &module);
        if (priority >= 0) {

            /* We have a component that indicated that it wants to run
               by giving us a module */
            avail = OBJ_NEW(avail_coll_t);
            avail->ac_priority = priority;
            avail->ac_module = module;

            /* Put this item on the list in priority order (lowest
               priority first).  Should it go first? */
            for (item2 = opal_list_get_first(selectable);
                 item2 != opal_list_get_end(selectable);
                 item2 = opal_list_get_next(item2)) {
                avail2 = (avail_coll_t *) item2;
                if (avail->ac_priority < avail2->ac_priority) {
                    opal_list_insert_pos(selectable,
                                         item2,
                                         (opal_list_item_t *) avail);
                    break;
                }
            }
            
            if (opal_list_get_end(selectable) == item2) {
                opal_list_append(selectable,
                                 (opal_list_item_t *) avail);
            }
        }
    }
    /*TODO: copy over any of the pointers */

    /* If we didn't find any available components, return an error */
    if (0 == opal_list_get_size(selectable)) {
        OBJ_RELEASE(selectable);
        return NULL;
    }

    /* All done */
    return selectable;
}


/*
 * Check a single component
 */
static int check_one_component(oshmem_group_t * group,
                               const mca_base_component_t * component,
                               mca_scoll_base_module_1_0_0_t ** module)
{
    int err;
    int priority = -1;

    err = query(component, group, &priority, module);

    if (OSHMEM_SUCCESS == err) {
        priority = (priority < 100) ? priority : 100;
        SCOLL_VERBOSE(10,"scoll:base:group_select: component available: %s, priority: %d",
                            component->mca_component_name, priority);

    } else {
        priority = -1;
        SCOLL_VERBOSE(10,"scoll:base:group_select: component not available: %s",
                            component->mca_component_name);
    }

    return priority;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a coll module, query it, and return the right
 * module struct
 */
static int query(const mca_base_component_t * component,
                 oshmem_group_t * group,
                 int *priority, mca_scoll_base_module_1_0_0_t ** module)
{
    *module = NULL;
    if (1 == component->mca_type_major_version &&
        0 == component->mca_type_minor_version &&
        0 == component->mca_type_release_version) {
        const mca_scoll_base_component_1_0_0_t *coll100 =
            (mca_scoll_base_component_1_0_0_t *) component;

        return query_1_0_0(coll100, group, priority, module);
    }

    /* Unknown coll API version -- return error */

    return OSHMEM_ERROR;
}


static int query_1_0_0(const mca_scoll_base_component_1_0_0_t * component,
                       oshmem_group_t * group, int *priority,
                       mca_scoll_base_module_1_0_0_t ** module)
{
    mca_scoll_base_module_1_0_0_t *ret;

    /* There's currently no need for conversion */

    ret = component->scoll_query(group, priority);
    if (NULL != ret) {
        *module = ret;
        return OSHMEM_SUCCESS;
    }

    return OSHMEM_ERROR;
}
