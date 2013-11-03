/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"
#include "sstore_central.h"

/*
 * Public string for version number
 */
const char *orte_sstore_central_component_version_string = 
    "ORTE SSTORE central MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int sstore_central_register (void);
static int sstore_central_open(void);
static int sstore_central_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_sstore_central_component_t mca_sstore_central_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itcentral
         */
        {
            ORTE_SSTORE_BASE_VERSION_2_0_0,
            /* Component name and version */
            "central",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            sstore_central_open,
            sstore_central_close,
            orte_sstore_central_component_query,
            sstore_central_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Verbosity level */
        0,
        /* opal_output handler */
        -1
    },
};

static int sstore_central_register (void)
{
    (void)mca_base_component_var_register(&mca_sstore_central_component.super.base_version,
                                          "verbose", "Verbose level for the SSTORE central component",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_sstore_central_component.super.verbose);

    /* Default priority */
    mca_sstore_central_component.super.priority = 20;
    (void)mca_base_component_var_register(&mca_sstore_central_component.super.base_version,
                                          "priority", "Priority of the SSTORE central component",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_sstore_central_component.super.priority);

    return ORTE_SUCCESS;
}

static int sstore_central_open(void) 
{
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_sstore_central_component.super.verbose) {
        mca_sstore_central_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_sstore_central_component.super.output_handle,
                                  mca_sstore_central_component.super.verbose);
    } else {
        mca_sstore_central_component.super.output_handle = orte_sstore_base_framework.framework_output;
    }
    
    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_sstore_central_component.super.output_handle,
                        "sstore:central: open()");
    opal_output_verbose(20, mca_sstore_central_component.super.output_handle,
                        "sstore:central: open: priority   = %d", 
                        mca_sstore_central_component.super.priority);
    opal_output_verbose(20, mca_sstore_central_component.super.output_handle,
                        "sstore:central: open: verbosity  = %d", 
                        mca_sstore_central_component.super.verbose);

    return ORTE_SUCCESS;
}

static int sstore_central_close(void)
{
    opal_output_verbose(10, mca_sstore_central_component.super.output_handle,
                        "sstore:central: close()");

    return ORTE_SUCCESS;
}
