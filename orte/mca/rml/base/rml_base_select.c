/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"

/**
 * Call the init function on all available components to find out if
 * they want to run.  Select the single component with the highest 
 * priority.
 */
int orte_rml_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    int selected_priority = -1;
    orte_rml_component_t *selected_component = NULL;
    orte_rml_module_t *selected_module = NULL;
    int wrapper_priority = -1;
    orte_rml_component_t *wrapper_component = NULL;
    orte_rml_module_t *wrapper_module = NULL;
    char *rml_wrapper = NULL;

    mca_base_param_reg_string_name("rml", "wrapper",
                                   "Use a Wrapper component around the selected RML component",
                                   false, false,
                                   NULL, &rml_wrapper);

    
    /* Traverse the list of opened modules; call their init functions. */
    for(item = opal_list_get_first(&orte_rml_base.rml_components);
        item != opal_list_get_end(&orte_rml_base.rml_components);
        item = opal_list_get_next(item)) {
        orte_rml_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rml_component_t *) cli->cli_component;

        opal_output_verbose(10, orte_rml_base.rml_output, 
            "orte_rml_base_select: initializing %s component %s",
            component->rml_version.mca_type_name,
            component->rml_version.mca_component_name);

        if (NULL == component->rml_init) {
          opal_output_verbose(10, orte_rml_base.rml_output, 
              "orte_rml_base_select: no init function; ignoring component");
        } else {
            int priority = 0;
            orte_rml_module_t* module = component->rml_init(&priority);

            /* 
             * If the component didn't initialize, remove it from the opened
             * list and remove it from the component repository 
             */
            if (NULL == module) {
                opal_output_verbose(10, orte_rml_base.rml_output,
                    "orte_rml_base_select: init returned failure");
                continue;
            }

            /*
             * If this is a wrapper component then save it for later
             */
            if(NULL != rml_wrapper &&
               RML_SELECT_WRAPPER_PRIORITY >= priority) {
                if( 0 == strncmp(component->rml_version.mca_component_name, 
                                 rml_wrapper,
                                 strlen(rml_wrapper) ) ) {
                    wrapper_priority  = priority;
                    wrapper_component = component;
                    wrapper_module    = module;
                }
            }
            /*
             * Otherwise this is a normal module and subject to normal selection
             */
            else if(priority > selected_priority) {
                selected_priority = priority;
                selected_component = component;
                selected_module = module;
            }
        }
    }

    /* 
     * Unload all components that were not selected
     */
    item = opal_list_get_first(&orte_rml_base.rml_components);
    while(item != opal_list_get_end(&orte_rml_base.rml_components)) {
        opal_list_item_t* next = opal_list_get_next(item);
        orte_rml_component_t* component;

        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rml_component_t *) cli->cli_component;

        /* Keep it if it is the wrapper component */
        if( NULL != wrapper_component &&
            component == wrapper_component ) {
            item = next;
            continue;
        }
        /* Not the selected component */
        if(component != selected_component) {
            opal_output_verbose(10, orte_rml_base.rml_output,
                "orte_rml_base_select: module %s unloaded",
                component->rml_version.mca_component_name);

            mca_base_component_repository_release((mca_base_component_t *) component);
            opal_list_remove_item(&orte_rml_base.rml_components, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    /* setup reference to selected module */
    if(NULL != selected_module) {
        orte_rml           = *selected_module;
        orte_rml_component = *selected_component;
    }

    /* If a wrapper component was requested then 
     * Make sure it can switch out the selected module
     */
    if( NULL != wrapper_component) {
        wrapper_module->init();
    }

    if( NULL != rml_wrapper ) {
        free(rml_wrapper);
    }
    
    return ORTE_SUCCESS;
}

