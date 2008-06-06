/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_UNISTD_H
#include "unistd.h"
#endif

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
        opal_crs_base_none_close,
        opal_crs_base_none_query
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
        
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
    opal_crs_base_none_enable_checkpoint,

    /** Prelaunch */
    opal_crs_base_none_prelaunch,

    /** Register thread */
    opal_crs_base_none_reg_thread
};

int opal_crs_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_crs_base_component_t *best_component = NULL;
    opal_crs_base_module_t *best_module = NULL;
    char *include_list = NULL;
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
    if( OPAL_INT_TO_BOOL(int_value) ) {
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: Not selecting at this time!");
        return OPAL_SUCCESS;
    }

    /*
     * Register the framework MCA param and look up include list
     */
    mca_base_param_reg_string_name("crs", NULL,
                                   "Which CRS component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &include_list);

    if(NULL != include_list && 0 == strncmp(include_list, "none", strlen("none")) ){ 
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:select: Using %s component",
                            include_list);
        best_component = &none_component;
        best_module    = &none_module;
        /* JJH: Todo: Check if none is in the list */
        /* Close all components since none will be used */
        mca_base_components_close(0, /* Pass 0 to keep this from closing the output handle */
                                  &opal_crs_base_components_available,
                                  NULL);
        goto skip_select;
    }

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("crs", opal_crs_base_output,
                                        &opal_crs_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 skip_select:
    /* Save the winner */
    opal_crs_base_selected_component = *best_component;
    opal_crs = *best_module;

    /* Initialize the winner */
    if (NULL != best_module) {
        if (OPAL_SUCCESS != (ret = opal_crs.crs_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != include_list ) {
        free(include_list);
        include_list = NULL;
    }

    return exit_status;
}
