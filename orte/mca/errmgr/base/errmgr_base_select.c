/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"


/*
 * List of composite modules, ordered by priority
 */
opal_pointer_array_t orte_errmgr_base_modules;

struct orte_errmgr_base_select_module_t {
    mca_base_component_t *component;
    mca_base_module_t *module;
    int priority;
};
typedef struct orte_errmgr_base_select_module_t orte_errmgr_base_select_module_t;

int orte_errmgr_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    opal_list_item_t *item = NULL;
    int priority = 0, i, j, low_i;
    orte_errmgr_base_select_module_t *tmp_module = NULL, *tmp_module_sw = NULL;
    opal_pointer_array_t tmp_array;
    orte_errmgr_base_module_t *i_module = NULL;

    /*
     * If the user does not want the recovery features, then do not select any.
     */
    if( !orte_errmgr_base_enable_recovery ) {
        goto INIT;
    }

    OBJ_CONSTRUCT(&tmp_array, opal_pointer_array_t);

    opal_output_verbose(10, orte_errmgr_base_output,
                        "errmgr:base:select: Auto-selecting components");

    /*
     * Traverse the list of available components.
     * For each call their 'query' functions to determine relative priority.
     */
    for (item  = opal_list_get_first(&orte_errmgr_base_components_available);
         item != opal_list_get_end(&orte_errmgr_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;

        /*
         * If there is a query function then use it.
         */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, orte_errmgr_base_output,
                                "errmgr:base:select Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }

        /*
         * Query this component for the module and priority
         */
        opal_output_verbose(5, orte_errmgr_base_output,
                            "errmgr:base:select Querying component [%s]",
                            component->mca_component_name);

        component->mca_query_component(&module, &priority);

        /*
         * If no module was returned or negative priority, then skip component
         */
        if (NULL == module || priority < 0) {
            opal_output_verbose(5, orte_errmgr_base_output,
                                "errmgr:base:select Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }

        /*
         * Append them to the temporary list, we will sort later
         */
        opal_output_verbose(5, orte_errmgr_base_output,
                            "errmgr:base:select Query of component [%s] set priority to %d", 
                            component->mca_component_name, priority);
        tmp_module = (orte_errmgr_base_select_module_t *)malloc(sizeof(orte_errmgr_base_select_module_t));
        tmp_module->component = component;
        tmp_module->module    = module;
        tmp_module->priority  = priority;

        opal_pointer_array_add(&tmp_array, (void*)tmp_module);
    }

    /*
     * Sort the list by decending priority
     */
    priority = 0;
    for(j = 0; j < tmp_array.size; ++j) {
        tmp_module_sw = (orte_errmgr_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, j);
        if( NULL == tmp_module_sw ) {
            continue;
        }

        low_i   = -1;
        priority = tmp_module_sw->priority;

        for(i = 0; i < tmp_array.size; ++i) {
            tmp_module = (orte_errmgr_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, i);
            if( NULL == tmp_module ) {
                continue;
            }
            if( tmp_module->priority > priority ) {
                low_i = i;
                priority = tmp_module->priority;
            }
        }

        if( low_i >= 0 ) {
            tmp_module = (orte_errmgr_base_select_module_t*)opal_pointer_array_get_item(&tmp_array, low_i);
            opal_pointer_array_set_item(&tmp_array, low_i, NULL);
            j--; /* Try this entry again, if it is not the lowest */
        } else {
            tmp_module = tmp_module_sw;
            opal_pointer_array_set_item(&tmp_array, j, NULL);
        }
        opal_output_verbose(5, orte_errmgr_base_output,
                            "errmgr:base:select Add module with priority [%s] %d", 
                            tmp_module->component->mca_component_name, tmp_module->priority);
        opal_pointer_array_add(&orte_errmgr_base_modules, (void*)(tmp_module->module));
        free(tmp_module);
    }
    OBJ_DESTRUCT(&tmp_array);

 INIT:
    /*
     * Initialize each of the Errmgr Modules
     */
    for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
        i_module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
        if( NULL == i_module ) {
            continue;
        }
        if( NULL != i_module->internal_errmgr_init ) {
            i_module->internal_errmgr_init();
        }
    }

    return exit_status;
}
