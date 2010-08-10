/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_example.h"

/*
 * Public string for version number
 */
const char *orte_errmgr_example_component_version_string = 
    "ORTE ERRMGR Example MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int errmgr_example_open(void);
static int errmgr_example_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_errmgr_example_component_t mca_errmgr_example_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itexample
         */
        {
            ORTE_ERRMGR_BASE_VERSION_3_0_0,
            /* Component name and version */
            "example",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            errmgr_example_open,
            errmgr_example_close,
            orte_errmgr_example_component_query
        },
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
    }
};

static int errmgr_example_open(void) 
{
    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_base_param_reg_int(&mca_errmgr_example_component.super.base_version,
                           "priority",
                           "Priority of the ERRMGR example component",
                           false, false,
                           mca_errmgr_example_component.super.priority,
                           &mca_errmgr_example_component.super.priority);
    
    mca_base_param_reg_int(&mca_errmgr_example_component.super.base_version,
                           "verbose",
                           "Verbose level for the ERRMGR example component",
                           false, false,
                           mca_errmgr_example_component.super.verbose, 
                           &mca_errmgr_example_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_errmgr_example_component.super.verbose) {
        mca_errmgr_example_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_errmgr_example_component.super.output_handle,
                                  mca_errmgr_example_component.super.verbose);
    } else {
        mca_errmgr_example_component.super.output_handle = orte_errmgr_base.output;
    }

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example: open()");
    opal_output_verbose(20, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example: open: priority      = %d", 
                        mca_errmgr_example_component.super.priority);
    opal_output_verbose(20, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example: open: verbosity     = %d", 
                        mca_errmgr_example_component.super.verbose);

    return ORTE_SUCCESS;
}

static int errmgr_example_close(void)
{
    opal_output_verbose(10, mca_errmgr_example_component.super.output_handle,
                        "errmgr:example: close()");

    return ORTE_SUCCESS;
}
