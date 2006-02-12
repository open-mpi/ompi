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

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "orte/orte_constants.h"
#include "orte/mca/sds/base/base.h"

extern opal_list_t orte_sds_base_components_available;
extern orte_sds_base_module_t *orte_sds_base_module;

int 
orte_sds_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    int selected_priority = -1;
    orte_sds_base_component_t *selected_component = NULL;
    orte_sds_base_module_t *selected_module = NULL;
 
    /* Traverse the list of opened modules; call their init functions. */
    for(item = opal_list_get_first(&orte_sds_base_components_available);
        item != opal_list_get_end(&orte_sds_base_components_available);
        item = opal_list_get_next(item)) {
        orte_sds_base_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_sds_base_component_t *) cli->cli_component;

        opal_output_verbose(10, 0, 
            "orte_sds_base_select: initializing %s component %s",
            component->sds_version.mca_type_name,
            component->sds_version.mca_component_name);

        if (NULL == component->sds_init) {
          opal_output_verbose(10, 0, 
              "orte_sds_base_select: no init function; ignoring component");
        } else {
            int priority;
            orte_sds_base_module_t* module = component->sds_init(&priority);

            /* If the component didn't initialize, remove it from the opened
               list and remove it from the component repository */
            if (NULL == module) {
                opal_output_verbose(10, 0,
                    "orte_sds_base_select: init returned failure");
                continue;
            }

            if(priority > selected_priority) {
                selected_priority = priority;
                selected_component = component;
                selected_module = module;
            }
        }
    }

    if (NULL == selected_component) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* unload all components that were not selected */
    item = opal_list_get_first(&orte_sds_base_components_available);
    while(item != opal_list_get_end(&orte_sds_base_components_available)) {
        opal_list_item_t* next = opal_list_get_next(item);
        orte_sds_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_sds_base_component_t *) cli->cli_component;
        if(component != selected_component) {
            opal_output_verbose(10, 0,
                "orte_sds_base_select: module %s unloaded",
                component->sds_version.mca_component_name);
            mca_base_component_repository_release((mca_base_component_t *) component);
            opal_list_remove_item(&orte_sds_base_components_available, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    /* setup reference to selected module */
    if(NULL != selected_module) {
        orte_sds_base_module = selected_module;
    }
    return ORTE_SUCCESS;
}
