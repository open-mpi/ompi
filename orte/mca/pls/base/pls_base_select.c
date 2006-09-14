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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/pls/base/base.h"


/**
* Function for selecting one component from all those that are
 * available.
 */

int orte_pls_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_pls_base_component_t *component, *best_component = NULL;
    orte_pls_base_module_t *module, *best_module = NULL;
    int priority, best_priority = -1;

    /* Query all the opened components and see if they want to run */

    for (item = opal_list_get_first(&orte_pls_base.available_components); 
         opal_list_get_end(&orte_pls_base.available_components) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_pls_base_component_t *) cli->cli_component;
        opal_output(orte_pls_base.pls_output,
                    "orte:base:open: querying component %s", 
                    component->pls_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->pls_init(&priority);

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
                opal_output(orte_pls_base.pls_output,
                        "orte:base:open: component %s does NOT want to be considered for selection", 
                        component->pls_version.mca_component_name);
                if (NULL == module->finalize) {
                    opal_output(orte_pls_base.pls_output,
                                "It appears you are the victim of a stale library - please delete your installation lib directory and reinstall");
                } else {
                    module->finalize();
                }
            }
        }
    }

    /* If we didn't find one to select, barf */

    if (NULL == best_component) {
        return ORTE_ERROR;
    }

    /* We have happiness -- save the component and module for later
       usage */

    orte_pls = *best_module;
    orte_pls_base.selected_component = *best_component;

    return ORTE_SUCCESS;
}
