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

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

orte_iof_base_module_t orte_iof;

/**
 * Call the init function on all available components to find out if
 * they want to run.  Select the single component with the highest 
 * priority.
 */
int orte_iof_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    int selected_priority = -1;
    orte_iof_base_component_t *selected_component = NULL;
    orte_iof_base_module_t *selected_module = NULL;
    bool selected_allow_user;
    bool selected_have_hidden;
 
    /* Traverse the list of opened modules; call their init functions. */
    for(item = opal_list_get_first(&orte_iof_base.iof_components_opened);
        item != opal_list_get_end(&orte_iof_base.iof_components_opened);
        item = opal_list_get_next(item)) {
        orte_iof_base_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_iof_base_component_t *) cli->cli_component;

        opal_output_verbose(10, orte_iof_base.iof_output, 
            "orte_iof_base_select: initializing %s component %s",
            component->iof_version.mca_type_name,
            component->iof_version.mca_component_name);

        if (NULL == component->iof_init) {
          opal_output_verbose(10, orte_iof_base.iof_output, 
              "orte_iof_base_select: no init function; ignoring component");
        } else {
            bool allow_user;
            bool have_hidden;
            int priority;
            orte_iof_base_module_t* module = component->iof_init(&priority, &allow_user, &have_hidden);

            /* If the component didn't initialize, remove it from the opened
               list and remove it from the component repository */
            if (NULL == module) {
                opal_output_verbose(10, orte_iof_base.iof_output,
                    "orte_iof_base_select: init returned failure");
                continue;
            }

            if(priority > selected_priority) {
                selected_priority = priority;
                selected_component = component;
                selected_module = module;
                selected_allow_user = allow_user;
                selected_have_hidden = have_hidden;
            }
        }
    }

    /* unload all components that were not selected */
    item = opal_list_get_first(&orte_iof_base.iof_components_opened);
    while(item != opal_list_get_end(&orte_iof_base.iof_components_opened)) {
        opal_list_item_t* next = opal_list_get_next(item);
        orte_iof_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_iof_base_component_t *) cli->cli_component;
        if(component != selected_component) {
            opal_output_verbose(10, orte_iof_base.iof_output,
                "orte_iof_base_select: module %s unloaded",
                component->iof_version.mca_component_name);
            mca_base_component_repository_release((mca_base_component_t *) component);
            opal_list_remove_item(&orte_iof_base.iof_components_opened, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    /* setup reference to selected module */
    if (NULL != selected_module) {
        orte_iof = *selected_module;
        orte_iof_base.iof_flush = true;
        return ORTE_SUCCESS;
    }

    /* Oops -- this shouldn't happen */

    opal_output(orte_iof_base.iof_output, "iof:select: no components found!");
    return ORTE_ERR_OUT_OF_RESOURCE;
}

