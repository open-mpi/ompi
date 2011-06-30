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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"


struct orte_sensor_base_select_module_t {
    mca_base_component_t *component;
    mca_base_module_t *module;
    int priority;
};
typedef struct orte_sensor_base_select_module_t orte_sensor_base_select_module_t;


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
    orte_sensor_base_module_t *i_module;
    opal_list_item_t *item;
    int priority = 0, i, j, low_i;
    int exit_status = OPAL_SUCCESS;
    opal_pointer_array_t tmp_array;
    bool none_found;
    orte_sensor_base_select_module_t *tmp_module = NULL, *tmp_module_sw = NULL;
    
    OBJ_CONSTRUCT(&tmp_array, opal_pointer_array_t);
    
    opal_output_verbose(10, orte_sensor_base.output,
                        "sensor:base:select: Auto-selecting components");
    
    /*
     * Traverse the list of available components.
     * For each call their 'query' functions to determine relative priority.
     */
    none_found = true;
    for (item  = opal_list_get_first(&mca_sensor_base_components_available);
         item != opal_list_get_end(&mca_sensor_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;
        
        /*
         * If there is a query function then use it.
         */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, orte_sensor_base.output,
                                "sensor:base:select Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }
        
        /*
         * Query this component for the module and priority
         */
        opal_output_verbose(5, orte_sensor_base.output,
                            "sensor:base:select Querying component [%s]",
                            component->mca_component_name);
        
        component->mca_query_component(&module, &priority);
        
        /*
         * If no module was returned or negative priority, then skip component
         */
        if (NULL == module || priority < 0) {
            opal_output_verbose(5, orte_sensor_base.output,
                                "sensor:base:select Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }
        
        /*
         * Append them to the temporary list, we will sort later
         */
        opal_output_verbose(5, orte_sensor_base.output,
                            "sensor:base:select Query of component [%s] set priority to %d", 
                            component->mca_component_name, priority);
        tmp_module = (orte_sensor_base_select_module_t *)malloc(sizeof(orte_sensor_base_select_module_t));
        tmp_module->component = component;
        tmp_module->module    = module;
        tmp_module->priority  = priority;
        
        opal_pointer_array_add(&tmp_array, (void*)tmp_module);
        none_found = false;
    }
    
    if (none_found) {
        /* okay for no modules to be found */
        return ORTE_SUCCESS;
    }
    
    /*
     * Sort the list by decending priority
     */
    priority = 0;
    for(j = 0; j < tmp_array.size; ++j) {
        tmp_module_sw = (orte_sensor_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, j);
        if( NULL == tmp_module_sw ) {
            continue;
        }
        
        low_i   = -1;
        priority = tmp_module_sw->priority;
        
        for(i = 0; i < tmp_array.size; ++i) {
            tmp_module = (orte_sensor_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, i);
            if( NULL == tmp_module ) {
                continue;
            }
            if( tmp_module->priority > priority ) {
                low_i = i;
                priority = tmp_module->priority;
            }
        }
        
        if( low_i >= 0 ) {
            tmp_module = (orte_sensor_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, low_i);
            opal_pointer_array_set_item(&tmp_array, low_i, NULL);
            j--; /* Try this entry again, if it is not the lowest */
        } else {
            tmp_module = tmp_module_sw;
            opal_pointer_array_set_item(&tmp_array, j, NULL);
        }
        opal_output_verbose(5, orte_sensor_base.output,
                            "sensor:base:select Add module with priority [%s] %d", 
                            tmp_module->component->mca_component_name, tmp_module->priority);
        opal_pointer_array_add(&orte_sensor_base.modules, (void*)(tmp_module->module));
        free(tmp_module);
    }
    OBJ_DESTRUCT(&tmp_array);
    
    /*
     * Initialize each of the modules in priority order from
     * highest to lowest
     */
    for(i = 0; i < orte_sensor_base.modules.size; ++i) {
        i_module = (orte_sensor_base_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i);
        if( NULL == i_module ) {
            continue;
        }
        if( NULL != i_module->init ) {
            if (ORTE_SUCCESS != i_module->init()) {
                /* can't run after all */
                opal_pointer_array_set_item(&orte_sensor_base.modules, i, NULL);
            }
        }
    }
    
    return exit_status;
}
