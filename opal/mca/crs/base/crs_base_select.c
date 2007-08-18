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

#include "opal_config.h"

#include "opal/include/opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal_cr.h"

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
static opal_crs_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        OPAL_CRS_BASE_VERSION_1_0_0,
        /* Component name and version */
        "none",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
            
        /* Component open and close functions */
        opal_crs_base_none_open,
        opal_crs_base_none_close
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
    0
};

static opal_crs_base_module_t none_module = {
    /** Initialization Function */
    opal_crs_base_none_module_init,
    /** Finalization Function */
    opal_crs_base_none_module_finalize,

    /** Checkpoint interface */
    opal_crs_base_none_checkpoint,

    /** Restart Command Access */
    opal_crs_base_none_restart,

    /** Disable checkpoints */
    opal_crs_base_none_disable_checkpoint,
    /** Enable checkpoints */
    opal_crs_base_none_enable_checkpoint
};

int opal_crs_base_select(void)
{
    int priority = 0, best_priority = -1;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    opal_crs_base_component_t *component = NULL, *best_component = NULL;
    opal_crs_base_module_t *module = NULL, *best_module = NULL;
    char *crs_include_list = NULL;
    bool fail_on_non_selection = false;
    bool do_not_select = false;
    int int_value = 0;

    /*
     * Note: If we are a tool, then we will manually run the selection routine 
     *       for the checkpointer.  The tool will set the MCA parameter 
     *       'crs_base_do_not_select' before opal_init and then reset it after to 
     *       disable the selection logic.
     *       This is useful for opal_restart because it reads the metadata file
     *       that indicates the checkpointer to be used after calling opal_init.
     *       Therefore it would need to select a specific module, but it doesn't
     *       know which one until later. It will set the MCA parameter 'crs' 
     *       before calling this function.
     */
    mca_base_param_reg_int_name("crs", 
                                "base_do_not_select",
                                "Do not do the selection of the CRS component",
                                true, false,
                                false, 
                                &int_value);
    if(0 != int_value)
        do_not_select = true;
    else 
        do_not_select = false;

    if(do_not_select) {
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: Not selecting at this time!");
        return OPAL_SUCCESS;
    }

    /* Register the framework MCA param and look it up */
    mca_base_param_reg_string_name("crs", NULL,
                                   "Which CRS component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &crs_include_list);

    if (NULL == crs_include_list || 0 == strlen(crs_include_list)) {
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: auto-selecting");
    } else {
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: looking for %s component", crs_include_list);
        if(0 == strncmp(crs_include_list, "none", strlen("none")) ) {
            goto do_none_comp;
        }
        else {
            /* If we can't find the requested component, then fail */
            fail_on_non_selection = true;
        }
    }
    
    /* Traverse the list of available components;
     * calling their init functions
     */
    for (item  = opal_list_get_first(&opal_crs_base_components_available);
         item != opal_list_get_end(&opal_crs_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (opal_crs_base_component_t *) cli->cli_component;
        
        /* If there is an include list -
         * the item must be in the list to be included :)
         */
        if (NULL != crs_include_list && 
            0 < strlen(crs_include_list) &&
            0 != strncmp(component->crs_version.mca_component_name,
                        crs_include_list, strlen(crs_include_list)) ) {
            opal_output_verbose(10, opal_crs_base_output,
                                "crs:select: Skipping %s component",
                                component->crs_version.mca_component_name);
            continue;
        }
        
        if (NULL == component->crs_query) {
            opal_output_verbose(10, opal_crs_base_output,
                                "crs:select: No init function! Ignoring component %s",
                                component->crs_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: Initializing component %s",
                            component->crs_version.mca_component_name);

        module = component->crs_query(&priority);
        if (NULL == module) {
            opal_output_verbose(10, opal_crs_base_output,
                                "crs:select: Init returned failure for component %s",
                                component->crs_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: Init returned priority %d", 
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
    if (NULL == best_component) {
        if( fail_on_non_selection ) {
            return OPAL_ERROR;
        }
        else {
            opal_output_verbose(19, opal_crs_base_output,
                                "crs:select: No component found, using the base component.  ;(");
            best_component = &none_component;
            best_module    = &none_module;
        }
    }

    /* Go through the list and close
     * the non-selected components
     */
    mca_base_components_close(0, /* We must pass it 0, to keep it from closing it */
                              &opal_crs_base_components_available,
                              (mca_base_component_t *) best_component);

    /* Save the winner */
    opal_crs_base_selected_component = *best_component;
    opal_crs = *best_module;
    opal_output_verbose(5, opal_crs_base_output,
                        "crs:select: Component %s selected",
                        best_component->crs_version.mca_component_name);

    /* Initialize the winner */
    if (NULL != best_module) {
        if (OPAL_SUCCESS != opal_crs.crs_init()) {
            return OPAL_ERROR;
        }
    }

    if( NULL != crs_include_list ) {
        free(crs_include_list);
        crs_include_list = NULL;
    }
    
    return OPAL_SUCCESS;
}
