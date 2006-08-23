/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"

/*
 * Globals
 */
bool opal_maffinity_base_selected = false;
const opal_maffinity_base_component_1_0_0_t *opal_maffinity_base_component = NULL;
const opal_maffinity_base_module_1_0_0_t *opal_maffinity_base_module = NULL;


int opal_maffinity_base_select(void)
{
    int priority = 0, best_priority = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    opal_maffinity_base_component_1_0_0_t *component = NULL,
        *best_component = NULL;
    const opal_maffinity_base_module_1_0_0_t *module = NULL, 
        *best_module = NULL;
    char *value;

    /* Register the framework MCA param and look it up */

    mca_base_param_reg_string_name("maffinity", NULL,
                                   "Which maffinity component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &value);
    if (NULL == value || 0 == strlen(value)) {
        opal_output_verbose(10, opal_maffinity_base_output,
                            "maffinity:select: auto-selecting");
    } else {
        opal_output_verbose(10, opal_maffinity_base_output,
                            "maffinity:select: looking for %s component", 
                            value);
    }

    /* Traverse the list of available components; call their init
       functions. */

    best_priority = -1;
    best_component = NULL;
    module = NULL;
    for (item = opal_list_get_first(&opal_maffinity_base_components_opened);
         opal_list_get_end(&opal_maffinity_base_components_opened) != item;
         item = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (opal_maffinity_base_component_1_0_0_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to
           be included */

        if (NULL != value && strlen(value) > 0 &&
            0 != strcmp(component->maffinityc_version.mca_component_name, 
                        value)) {
            opal_output_verbose(10, opal_maffinity_base_output,
                                 "maffinity:select: skipping %s component",
                                 component->maffinityc_version.mca_component_name);
            continue;
        }

        if (NULL == component->maffinityc_query) {
            opal_output_verbose(10, opal_maffinity_base_output,
                                "pafinity:select: no init function; ignoring component %s",
                                component->maffinityc_version.mca_component_name );
            continue;
        }
        opal_output_verbose(10, opal_maffinity_base_output, 
                            "maffinity:select: initializing component %s",
                            component->maffinityc_version.mca_component_name);
        module = component->maffinityc_query(&priority);
        if (NULL == module) {
            opal_output_verbose(10, opal_maffinity_base_output,
                                "maffinity:select: init returned failure for component %s",
                                component->maffinityc_version.mca_component_name );
            continue;
        }
        opal_output_verbose(10, opal_maffinity_base_output,
                            "maffinity:select: init returned priority %d", 
                            priority );
        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }
    }

    /* Finished querying all components.  Check for the bozo case. */

    if (NULL == best_component ) {
        return OPAL_ERR_NOT_FOUND;
    } 

    /* Now go through the opened list and close all the non-selected
       components */

    mca_base_components_close(opal_maffinity_base_output, 
                              &opal_maffinity_base_components_opened, 
                              (mca_base_component_t *) best_component);
    
    /* Save the winner */

    opal_maffinity_base_component = best_component;
    opal_maffinity_base_module = best_module;
    opal_output_verbose(10, opal_maffinity_base_output, 
                        "maffinity:select: component %s selected",
                        best_component->maffinityc_version.mca_component_name);
    opal_maffinity_base_selected = true;

    /* Initialize the winner */

    if (NULL != opal_maffinity_base_module) {
        if (OPAL_SUCCESS != opal_maffinity_base_module->maff_module_init()) {
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

