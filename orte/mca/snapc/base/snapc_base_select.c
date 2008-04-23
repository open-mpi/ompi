/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"


static orte_snapc_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        ORTE_SNAPC_BASE_VERSION_1_0_0,
        /* Component name and version */
        "none",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        
        /* Component open and close functions */
        orte_snapc_base_none_open,
        orte_snapc_base_none_close
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    
    /* Query Function */
    NULL,
    /* Verbosity level */
    0,
    /* opal_output handler */
    -1,
    /* Default priority */
    1
};

static orte_snapc_base_module_t none_module = {
    /** Initialization Function */
    orte_snapc_base_module_init,
    /** Finalization Function */
    orte_snapc_base_module_finalize,
    orte_snapc_base_none_setup_job,
    orte_snapc_base_none_release_job,
    orte_snapc_base_none_ft_event
};

int orte_snapc_base_select(bool seed, bool app)
{
    int priority = 0, best_priority = -1;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    orte_snapc_base_component_t *component = NULL, *best_component = NULL;
    orte_snapc_base_module_t *module = NULL, *best_module = NULL;
    char *snapc_include_list = NULL;
    bool fail_on_non_selection = false;

    /* Register the framework MCA param and look it up */
    mca_base_param_reg_string_name("snapc", NULL,
                                   "Which SNAPC component to use (empty = auto-select)",
                                   false, false,
                                   strdup("none"), &snapc_include_list);

    if (NULL == snapc_include_list || 0 == strlen(snapc_include_list)) {
        opal_output_verbose(25, orte_snapc_base_output,
                            "snapc:select: auto-selecting");
    } else {
        opal_output_verbose(25, orte_snapc_base_output,
                            "snapc:select: looking for %s component", snapc_include_list);
        if(0 == strncmp(snapc_include_list, "none", strlen("none")) ) {
            goto do_none_comp;
        }
        else {
            fail_on_non_selection = true;
        }
    }
    
    /* Traverse the list of available components;
     * calling their init functions
     */
    for (item  = opal_list_get_first(&orte_snapc_base_components_available);
         item != opal_list_get_end(&orte_snapc_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_snapc_base_component_t *) cli->cli_component;
        
        /* If there is an include list -
         * the item must be in the list to be included :)
         */
        if (NULL != snapc_include_list &&
            0 < strlen(snapc_include_list) &&
            0 != strncmp(component->snapc_version.mca_component_name,
                         snapc_include_list, strlen(snapc_include_list)) ) {
            opal_output_verbose(25, orte_snapc_base_output,
                                "snapc:select: Skipping %s component",
                                component->snapc_version.mca_component_name);
            continue;
        }
        
        if (NULL == component->snapc_query) {
            opal_output_verbose(25, orte_snapc_base_output,
                                "snapc:select: No init function! Ignoring component %s",
                                component->snapc_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(25, orte_snapc_base_output,
                            "snapc:select: Initializing component %s",
                            component->snapc_version.mca_component_name);

        module = component->snapc_query(&priority);
        if (NULL == module) {
            opal_output_verbose(25, orte_snapc_base_output,
                                "snapc:select: Init returned failure for component %s",
                                component->snapc_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(25, orte_snapc_base_output,
                            "snapc:select: Init returned priority %d", 
                            priority);
        if (priority > best_priority) {
            best_priority  = priority;
            best_component = component;
            best_module    = module;
        }
    }
    
    /* Finished querying all components.
     * Check for the bozo case.
     */
 do_none_comp:
    if (NULL == best_component ) {
        if( fail_on_non_selection ) {
            return ORTE_ERROR;
        }
        else {
            opal_output_verbose(25, orte_snapc_base_output,
                                "snapc:select: No component found, using the base component.  ;(");
            best_component = &none_component;
            best_module    = &none_module;
        }
    }
    
    /* Go through the list and close
     * the non-selected components
     */
    mca_base_components_close(orte_snapc_base_output,
                              &orte_snapc_base_components_available,
                              (mca_base_component_t *) best_component);

    /* Save the winner */
    orte_snapc_base_selected_component = *best_component;
    orte_snapc = *best_module;
    opal_output_verbose(15, orte_snapc_base_output,
                        "snapc:select: Component %s selected",
                        best_component->snapc_version.mca_component_name);

    /* Initialize the winner */
    if (NULL != best_module) {
        if (ORTE_SUCCESS != orte_snapc.snapc_init( seed, app )) {
            return ORTE_ERROR;
        }
    }

    if( NULL != snapc_include_list ) {
        free(snapc_include_list);
        snapc_include_list = NULL;
    }

    return ORTE_SUCCESS;
}
