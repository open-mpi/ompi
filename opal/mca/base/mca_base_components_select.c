/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/util/strncpy.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"


int mca_base_select(const char *type_name, int output_id,
                    opal_list_t *components_available,
                    mca_base_module_t **best_module,
                    mca_base_component_t **best_component)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    opal_list_item_t *item = NULL;
    int priority = 0, best_priority = -1;
    char *include_list = NULL;
    char *tmp_str = NULL;

    *best_module = NULL;
    *best_component = NULL;

    /*
     * See if there is an include list
     */
    asprintf(&tmp_str, "Which %s component to use (empty = auto-select)", type_name);
    mca_base_param_reg_string_name(type_name, NULL, tmp_str,
                                   false, false,
                                   NULL, &include_list);
    free(tmp_str);
    tmp_str = NULL;
    if (NULL == include_list || 0 == strlen(include_list)) {
        opal_output_verbose(10, output_id,
                            "%s:select: Auto-selecting",
                            type_name);
    } else {
        opal_output_verbose(10, output_id,
                            "%s:select: Selecting from %s component(s)",
                            type_name, include_list);
    }

    /*
     * Traverse the list of available components.
     * For each call their 'query' functions to determine relative priority.
     */
    for (item  = opal_list_get_first(components_available);
         item != opal_list_get_end(components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;

        /*
         * If there is an include list, make sure this item is in the list
         */
        if( NULL != include_list && 0 < strlen(include_list) ) {
            if( 0 != strncmp(component->mca_component_name,
                             include_list,
                             strlen(include_list)) ) {
                opal_output_verbose(5, output_id,
                                    "mca:base:select: Skipping component [%s]. Not in the include list [%s]",
                                    component->mca_component_name,
                                    include_list);
                continue;
            }
        }

        /*
         * If there is a query function then use it.
         */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, output_id,
                                "mca:base:select: Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }

        /*
         * Query this component for the module and priority
         */
        opal_output_verbose(5, output_id,
                            "mca:base:select: Querying component [%s]",
                            component->mca_component_name);

        component->mca_query_component(&module, &priority);

        /*
         * If no module was returned, then skip component
         */
        if (NULL == module) {
            opal_output_verbose(5, output_id,
                                "mca:base:select: Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }

        /*
         * Determine if this is the best module we have seen by looking the priority
         */
        opal_output_verbose(5, output_id,
                            "mca:base:select: Query of component [%s] set priority to %d", 
                            component->mca_component_name,
                            priority);
        if (priority > best_priority) {
            best_priority  = priority;
            *best_component = component;
            *best_module    = module;
        }
    }


    /*
     * Finished querying all components.
     * Make sure we found something in the process.
     */
    if (NULL == *best_component) {
        opal_output_verbose(5, output_id,
                            "mca:base:select: No component selected!");
        /*
         * Still close the non-selected components
         */
        mca_base_components_close(0, /* Pass 0 to keep this from closing the output handle */
                                  components_available,
                                  NULL);
        return OPAL_ERROR;
    }

    opal_output_verbose(5, output_id,
                        "mca:base:select: Selected component [%s]",
                        (*best_component)->mca_component_name);

    /*
     * Close the non-selected components
     */
    mca_base_components_close(output_id,
                              components_available,
                              (mca_base_component_t *) (*best_component));


    if( NULL != include_list ) {
        free(include_list);
        include_list = NULL;
    }

    return OPAL_SUCCESS;
}
