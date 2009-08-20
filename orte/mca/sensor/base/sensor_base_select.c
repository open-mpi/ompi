/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"

#include "orte/mca/sensor/base/base.h"


/**
 * Function for weeding out sensor components that don't want to run.
 *
 * Call the init function on all available components to find out if
 * they want to run.  Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be returned to the caller in a opal_list_t.
 */
int orte_sensor_base_select(void)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    orte_sensor_base_module_t *nmodule;
    opal_list_item_t *item;
    int i, priority, ret;
    char *include_list = NULL;
    char **imodules = NULL;
    orte_sensor_base_selected_pair_t *pair;
    
    /*
     * Register the framework MCA param and look up include list
     */
    mca_base_param_reg_string_name("sensor", NULL,
                                   "Comma-delimisted list of  sensor component to use (empty = all avail)",
                                   false, false,
                                   NULL, &include_list);
    
    /* if the list is empty, then we have nothing to do */
    if (NULL == include_list) {
        return ORTE_SUCCESS;
    }
    
    /* separate the names of the sensors to be used */
    imodules = opal_argv_split(include_list, ',');

    /* Query all available components and ask if they have a module */
    for (item = opal_list_get_first(&mca_sensor_base_components_available);
         opal_list_get_end(&mca_sensor_base_components_available) != item;
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;
        
        /* If this component was not specified, skip it */
        for (i = 0; NULL != imodules[i]; ++i) {
            if (0 == strcmp(imodules[i], component->mca_component_name)) {
                break;
            }
        }
        if (NULL == imodules[i]) {
            continue;
        }
        
        /* If there's no query function, skip it */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, orte_sensor_base_output,
                                "mca:sensor:select: Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }
        
        /* Query the component */
        opal_output_verbose(5, orte_sensor_base_output,
                            "mca:sensor:select: Querying component [%s]",
                            component->mca_component_name);
        ret = component->mca_query_component(&module, &priority);
        
        /* If no module was returned, then skip component */
        if (ORTE_SUCCESS != ret || NULL == module) {
            opal_output_verbose(5, orte_sensor_base_output,
                                "mca:sensor:select: Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }
        
        /* If we got a module, initialize it */
        nmodule = (orte_sensor_base_module_t*) module;
        if (NULL != nmodule->init) {
            /* If the module doesn't want to be used, skip it */
            if (ORTE_SUCCESS != (ret = nmodule->init()) ) {
                if (NULL != nmodule->finalize) {
                    nmodule->finalize();
                }
                continue;
            }
        }
        
        opal_output_verbose(5, orte_sensor_base_output,
                            "mca:sensor:select: Adding component [%s] to active list",
                            component->mca_component_name );

        /* Make an item for the list */
        pair = OBJ_NEW(orte_sensor_base_selected_pair_t);
        pair->component = (orte_sensor_base_component_t*) component;
        pair->module = nmodule;
        
        /* Add it to the list of operational sensors */
        opal_list_append(&orte_sensor_base_selected_modules, &(pair->super));
    }

    return ORTE_SUCCESS;
}
