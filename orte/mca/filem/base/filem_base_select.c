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

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"


static orte_filem_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        ORTE_FILEM_BASE_VERSION_1_0_0,
        /* Component name and version */
        "none",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        
        /* Component open and close functions */
        orte_filem_base_none_open,
        orte_filem_base_none_close
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Is the component checkpointable ? */
        true
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

static orte_filem_base_module_t none_module = {
    /** Initialization Function */
    orte_filem_base_module_init,
    /** Finalization Function */
    orte_filem_base_module_finalize,

    orte_filem_base_none_put,
    orte_filem_base_none_get,
    orte_filem_base_none_rm
};

int orte_filem_base_select(void)
{
    int priority = 0, best_priority = -1;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    orte_filem_base_component_t *component = NULL, *best_component = NULL;
    orte_filem_base_module_t *module = NULL, *best_module = NULL;
    char *filem_include_list = NULL;
    bool fail_on_non_selection = false;

    /* Register the framework MCA param and look it up */
    mca_base_param_reg_string_name("filem", NULL,
                                   "Which FileM component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &filem_include_list);
    
    if (NULL == filem_include_list || 0 == strlen(filem_include_list)) {
        opal_output_verbose(10, orte_filem_base_output,
                            "filem:select: auto-selecting");
    } else {
        opal_output_verbose(10, orte_filem_base_output,
                            "filem:select: looking for %s component", filem_include_list);
        if(0 == strncmp(filem_include_list, "none", strlen("none")) ) {
            goto do_none_comp;
        }
        else {
            fail_on_non_selection = true;
        }
    }
    
    /* Traverse the list of available components;
     * calling their init functions
     */
    for (item  = opal_list_get_first(&orte_filem_base_components_available);
         item != opal_list_get_end(&orte_filem_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_filem_base_component_t *) cli->cli_component;
        
        /* If there is an include list -
         * the item must be in the list to be included :)
         */
        if (NULL != filem_include_list     && 
            0 < strlen(filem_include_list) &&
            0 != strncmp(component->filem_version.mca_component_name, 
                         filem_include_list, strlen(filem_include_list)) ) {
            opal_output_verbose(10, orte_filem_base_output,
                                "filem:select: Skipping %s component",
                                component->filem_version.mca_component_name);
            continue;
        }
        
        if (NULL == component->filem_query) {
            opal_output_verbose(10, orte_filem_base_output,
                                "filem:select: No init function! Ignoring component %s",
                                component->filem_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(10, orte_filem_base_output,
                            "filem:select: Initializing component %s",
                            component->filem_version.mca_component_name);

        module = component->filem_query(&priority);
        if (NULL == module) {
            opal_output_verbose(10, orte_filem_base_output,
                                "filem:select: Init returned failure for component %s",
                                component->filem_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(10, orte_filem_base_output,
                            "filem:select: Init returned priority %d", 
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
            opal_output_verbose(10, orte_filem_base_output,
                                "filem:select: No component found, using the base component.  ;(");
            best_component = &none_component;
            best_module    = &none_module;
        }
    }
    
    /* Go through the list and close
     * the non-selected components
     */
    mca_base_components_close(0, /* We must pass it 0, to keep it from closing it */
                              &orte_filem_base_components_available,
                              (mca_base_component_t *) best_component);

    /* Save the winner */
    orte_filem_base_selected_component = *best_component;
    orte_filem = *best_module;
    opal_output_verbose(5, orte_filem_base_output,
                        "filem:select: Component %s selected",
                        best_component->filem_version.mca_component_name);

    /* Initialize the winner */
    if (NULL != best_module) {
        if (ORTE_SUCCESS != orte_filem.filem_init() ) {
            return ORTE_ERROR;
        }
    }

    if( NULL != filem_include_list ) {
        free(filem_include_list);
    }
    return ORTE_SUCCESS;
}
