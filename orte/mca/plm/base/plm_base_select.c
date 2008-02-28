/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"


/**
* Function for selecting one component from all those that are
 * available.
 */

int orte_plm_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_plm_base_component_t *component, *best_component = NULL;
    orte_plm_base_module_t *module, *best_module = NULL;
    int priority, best_priority = -1;

    /* Query all the opened components and see if they want to run */

    for (item = opal_list_get_first(&orte_plm_base.available_components); 
         opal_list_get_end(&orte_plm_base.available_components) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_plm_base_component_t *) cli->cli_component;
        opal_output_verbose(10, orte_plm_globals.output,
                    "orte:base:select: querying component %s", 
                    component->plm_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->plm_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            /* If this is the best one, save it */
            if (priority > best_priority) {

                /* If there was a previous best one, finalize */
                if (NULL != best_module) {
                    best_module->finalize();
                }

                /* Save the new best one */
                best_module = module;
                best_component = component;

                /* update the best priority */
                best_priority = priority;
            } else {
                opal_output_verbose(10, orte_plm_globals.output,
                                    "orte:base:select: component %s does did not win the election",
                                    component->plm_version.mca_component_name);
                if (NULL == module->finalize) {
                    opal_output(orte_plm_globals.output,
                                "It appears you are the victim of a stale library - please delete your installation lib directory and reinstall");
                } else {
                    module->finalize();
                }
            }
        }
    }

    /* If we didn't find one, and we are a daemon, then default to retaining the proxy.
     * Otherwise, if we didn't find one to select, that is unacceptable. 
     */
    if (NULL == best_component) {
        if (orte_process_info.daemon) {
            /* don't record a selected component or flag selected
             * so we finalize correctly - just leave the plm alone
             * as it defaults to pointing at the proxy
             */
            goto cleanup;
        } else {
            /* barf */
            return ORTE_ERROR;
        }
    }

    /* We have happiness -- save the component and module for later
       usage */

    orte_plm = *best_module;
    orte_plm_base.selected_component = *best_component;
    orte_plm_base.selected = true;

cleanup:
    /* unload all components that were not selected */
    item = opal_list_get_first(&orte_plm_base.available_components);
    while(item != opal_list_get_end(&orte_plm_base.available_components)) {
        opal_list_item_t* next = opal_list_get_next(item);
        orte_plm_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_plm_base_component_t *) cli->cli_component;
        if(component != best_component) {
            opal_output_verbose(10, orte_plm_globals.output,
                                "orte_plm_base_select: module %s unloaded",
                                component->plm_version.mca_component_name);
            mca_base_component_repository_release((mca_base_component_t *) component);
            opal_list_remove_item(&orte_plm_base.available_components, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    return ORTE_SUCCESS;
}
