/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/rml/rml.h"
#include "mca/rml/base/base.h"


/**
 * Call the init function on all available components to find out if
 * they want to run.  Select the single component with the highest 
 * priority.
 */
int orte_rml_base_select(void)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    int selected_priority = -1;
    orte_rml_component_t *selected_component = NULL;
    orte_rml_module_t *selected_module = NULL;
 
    /* Traverse the list of opened modules; call their init functions. */
    for(item = ompi_list_get_first(&orte_rml_base.rml_components);
        item != ompi_list_get_end(&orte_rml_base.rml_components);
        item = ompi_list_get_next(item)) {
        orte_rml_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rml_component_t *) cli->cli_component;

        ompi_output_verbose(10, orte_rml_base.rml_output, 
            "orte_rml_base_select: initializing %s component %s",
            component->rml_version.mca_type_name,
            component->rml_version.mca_component_name);

        if (NULL == component->rml_init) {
          ompi_output_verbose(10, orte_rml_base.rml_output, 
              "orte_rml_base_select: no init function; ignoring component");
        } else {
            int priority;
            orte_rml_module_t* module = component->rml_init(&priority);

            /* If the component didn't initialize, remove it from the opened
               list and remove it from the component repository */
            if (NULL == module) {
                ompi_output_verbose(10, orte_rml_base.rml_output,
                    "orte_rml_base_select: init returned failure");
                continue;
            }

            if(priority > selected_priority) {
                selected_priority = priority;
                selected_component = component;
                selected_module = module;
            }
        }
    }

    /* unload all components that were not selected */
    item = ompi_list_get_first(&orte_rml_base.rml_components);
    while(item != ompi_list_get_end(&orte_rml_base.rml_components)) {
        ompi_list_item_t* next = ompi_list_get_next(item);
        orte_rml_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rml_component_t *) cli->cli_component;
        if(component != selected_component) {
            ompi_output_verbose(10, orte_rml_base.rml_output,
                "orte_rml_base_select: module %s unloaded",
                component->rml_version.mca_component_name);
            mca_base_component_repository_release((mca_base_component_t *) component);
            ompi_list_remove_item(&orte_rml_base.rml_components, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    /* setup reference to selected module */
    if(NULL != selected_module) {
        orte_rml = *selected_module;
    }
    return OMPI_SUCCESS;
}

