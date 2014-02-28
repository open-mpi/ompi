/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

/*
 * globals
 */
bool mca_sshmem_base_selected = false;
const mca_sshmem_base_component_2_0_0_t *mca_sshmem_base_component = NULL;
const mca_sshmem_base_module_2_0_0_t *mca_sshmem_base_module = NULL;

/* ////////////////////////////////////////////////////////////////////////// */
static int
mca_sshmem_base_runtime_query(mca_base_module_t **best_module,
                              mca_base_component_t **best_component)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    int priority = 0, best_priority = INT32_MIN;

    *best_module = NULL;
    *best_component = NULL;

    opal_output_verbose(10, oshmem_sshmem_base_framework.framework_output,
                        "sshmem: base: runtime_query: "
                        "Auto-selecting sshmem components");

    /* traverse the list of available components.
     * for each call their 'run-time query' functions to determine relative
     * priority.
     */
    OPAL_LIST_FOREACH(cli, &oshmem_sshmem_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_base_component_t *)cli->cli_component;

        /* if there is a run-time query function then use it. otherwise, skip
         * the component.
         */
        if (NULL == ((mca_sshmem_base_component_2_0_0_t *)
                     component)->runtime_query) {
            opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                                "sshmem: base: runtime_query: "
                                "(sshmem) Skipping component [%s]. It does not "
                                "implement a run-time query function",
                                component->mca_component_name);
            continue;
        }

        /* query this component for the module and priority */
        opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                            "sshmem: base: runtime_query: "
                            "(shmem) Querying component (run-time) [%s]",
                            component->mca_component_name);

        ((mca_sshmem_base_component_2_0_0_t *)
         component)->runtime_query(&module, &priority, NULL);

        /* if no module was returned, then skip component.
         * this probably means that the run-time test deemed the shared memory
         * backing facility unusable or unsafe.
         */
        if (NULL == module) {
            opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                                "sshmem: base: runtime_query: "
                                "(sshmem) Skipping component [%s]. Run-time "
                                "Query failed to return a module",
                                component->mca_component_name);
            continue;
        }

        /* determine if this is the best module we have seen by looking the
         * priority
         */
        opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                            "sshmem: base: runtime_query: "
                            "(%5s) Query of component [%s] set priority to %d",
                            "shmem", component->mca_component_name, priority);
        if (priority > best_priority) {
            best_priority = priority;
            *best_module = module;
            *best_component = component;
        }
    }

    /* finished querying all components.
     * make sure we found something in the process.
     */
    if (NULL == *best_component) {
        opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                            "sshmem: base: runtime_query: "
                            "(%5s) No component selected!", "shmem");
        return OSHMEM_ERR_NOT_FOUND;
    }

    opal_output_verbose(5, oshmem_sshmem_base_framework.framework_output,
                        "sshmem: base: runtime_query: "
                        "(%5s) Selected component [%s]", "shmem",
                        (*best_component)->mca_component_name);

    /* close the non-selected components */
    (void) mca_base_framework_components_close (&oshmem_sshmem_base_framework,
                                                (mca_base_component_t *)(*best_component));

    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_base_select(void)
{
    mca_sshmem_base_component_2_0_0_t *best_component = NULL;
    mca_sshmem_base_module_2_0_0_t *best_module = NULL;
    /* select the best component */
    if (OSHMEM_SUCCESS != mca_sshmem_base_runtime_query(
                                (mca_base_module_t **)&best_module,
                                (mca_base_component_t **)&best_component)) {
        /* it is NOT okay if we don't find a module because we need at
         * least one shared memory backing facility component instance.
         */
        return OSHMEM_ERROR;
    }

    /* save the winner */
    mca_sshmem_base_component = best_component;
    mca_sshmem_base_module    = best_module;
    mca_sshmem_base_selected  = true;

    /* initialize the winner */
    if (NULL != mca_sshmem_base_module) {
        return mca_sshmem_base_module->module_init();
    }
    else {
        return OSHMEM_ERROR;
    }
}

