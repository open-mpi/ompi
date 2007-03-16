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

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"


static ompi_crcp_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        OMPI_CRCP_BASE_VERSION_1_0_0,
        /* Component name and version */
        "none",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        
        /* Component open and close functions */
        ompi_crcp_base_none_open,
        ompi_crcp_base_none_close
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

static ompi_crcp_base_module_t none_module = {
    /** Initialization Function */
    ompi_crcp_base_module_init,
    /** Finalization Function */
    ompi_crcp_base_module_finalize,

    /** PML Wrapper */
    ompi_crcp_base_none_pml_enable,

    ompi_crcp_base_none_pml_add_comm,
    ompi_crcp_base_none_pml_del_comm,

    ompi_crcp_base_none_pml_add_procs,
    ompi_crcp_base_none_pml_del_procs,
    
    ompi_crcp_base_none_pml_progress,

    ompi_crcp_base_none_pml_iprobe,
    ompi_crcp_base_none_pml_probe,
    
    ompi_crcp_base_none_pml_isend_init,
    ompi_crcp_base_none_pml_isend,
    ompi_crcp_base_none_pml_send,
    
    ompi_crcp_base_none_pml_irecv_init,
    ompi_crcp_base_none_pml_irecv,
    ompi_crcp_base_none_pml_recv,
    
    ompi_crcp_base_none_pml_dump,
    ompi_crcp_base_none_pml_start,
    ompi_crcp_base_none_pml_ft_event,

    /** Request Wrapper */
    ompi_crcp_base_none_request_complete,

    /** BTL Wrapper */
    ompi_crcp_base_none_btl_add_procs,
    ompi_crcp_base_none_btl_del_procs,

    ompi_crcp_base_none_btl_register,
    ompi_crcp_base_none_btl_finalize,

    ompi_crcp_base_none_btl_alloc,
    ompi_crcp_base_none_btl_free,

    ompi_crcp_base_none_btl_prepare_src,
    ompi_crcp_base_none_btl_prepare_dst,

    ompi_crcp_base_none_btl_send,
    ompi_crcp_base_none_btl_put,
    ompi_crcp_base_none_btl_get,

    ompi_crcp_base_none_btl_dump,
    ompi_crcp_base_none_btl_ft_event    
};

int ompi_crcp_base_select(void)
{
    int priority = 0, best_priority = -1;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    ompi_crcp_base_component_t *component = NULL, *best_component = NULL;
    ompi_crcp_base_module_t *module = NULL, *best_module = NULL;
    char *crcp_include_list = NULL;
    bool fail_on_non_selection = false;

    /* Register the framework MCA param and look it up */
    mca_base_param_reg_string_name("crcp", NULL,
                                   "Which CRCP component to use (empty = auto-select)",
                                   false, false,
                                   strdup("none"), &crcp_include_list);

    if (NULL == crcp_include_list || 0 == strlen(crcp_include_list)) {
        opal_output_verbose(20, ompi_crcp_base_output,
                            "crcp:select: auto-selecting");
    } else {
        opal_output_verbose(20, ompi_crcp_base_output,
                            "crcp:select: looking for %s component", crcp_include_list);
        if(0 == strncmp(crcp_include_list, "none", strlen("none")) ) {
            goto do_none_comp;
        }
        else {
            fail_on_non_selection = true;
        }
    }
    
    /* Traverse the list of available components;
     * calling their init functions
     */
    for (item  = opal_list_get_first(&ompi_crcp_base_components_available);
         item != opal_list_get_end(&ompi_crcp_base_components_available);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (ompi_crcp_base_component_t *) cli->cli_component;
        
        /* If there is an include list -
         * the item must be in the list to be included :)
         */
        if (NULL != crcp_include_list && 
            0 < strlen(crcp_include_list) &&
            0 != strncmp(component->crcp_version.mca_component_name,
                         crcp_include_list, strlen(crcp_include_list)) ) {
            opal_output_verbose(20, ompi_crcp_base_output,
                                "crcp:select: Skipping %s component",
                                component->crcp_version.mca_component_name);
            continue;
        }
        
        if (NULL == component->crcp_query) {
            opal_output_verbose(20, ompi_crcp_base_output,
                                "crcp:select: No init function! Ignoring component %s",
                                component->crcp_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(20, ompi_crcp_base_output,
                            "crcp:select: Initializing component %s",
                            component->crcp_version.mca_component_name);

        module = component->crcp_query(&priority);
        if (NULL == module) {
            opal_output_verbose(20, ompi_crcp_base_output,
                                "crcp:select: Init returned failure for component %s",
                                component->crcp_version.mca_component_name );
            continue;
        }
        
        opal_output_verbose(20, ompi_crcp_base_output,
                            "crcp:select: Init returned priority %d", 
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
            return OMPI_ERROR;
        }
        else {
            opal_output_verbose(20, ompi_crcp_base_output,
                                "crcp:select: No component found, using the base component.  ;(");
            best_component = &none_component;
            best_module    = &none_module;
        }
    }
    
    /* Go through the list and close
     * the non-selected components
     */
    mca_base_components_close(0, /* We must pass it 0, to keep it from closing it */
                              &ompi_crcp_base_components_available,
                              (mca_base_component_t *) best_component);

    /* Save the winner */
    ompi_crcp_base_selected_component = *best_component;
    ompi_crcp = *best_module;
    opal_output_verbose(15, ompi_crcp_base_output,
                        "crcp:select: Component %s selected",
                        best_component->crcp_version.mca_component_name);

    /* Initialize the winner */
    if (NULL != best_module) {
        if (OMPI_SUCCESS != ompi_crcp.crcp_init(  )) {
            return OMPI_ERROR;
        }
    }

    if( NULL != crcp_include_list ) {
        free(crcp_include_list);
    }
    
    return OMPI_SUCCESS;
}
