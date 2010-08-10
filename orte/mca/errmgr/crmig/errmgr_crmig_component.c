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
#include "errmgr_crmig.h"

/*
 * Public string for version number
 */
const char *orte_errmgr_crmig_component_version_string = 
    "ORTE ERRMGR crmig MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int errmgr_crmig_open(void);
static int errmgr_crmig_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_errmgr_crmig_component_t mca_errmgr_crmig_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itcrmig
         */
        {
            ORTE_ERRMGR_BASE_VERSION_3_0_0,
            /* Component name and version */
            "crmig",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            errmgr_crmig_open,
            errmgr_crmig_close,
            orte_errmgr_crmig_component_query
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
        40
    }
};

static int errmgr_crmig_open(void) 
{
    int val;

    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_base_param_reg_int(&mca_errmgr_crmig_component.super.base_version,
                           "priority",
                           "Priority of the ERRMGR crmig component",
                           false, false,
                           mca_errmgr_crmig_component.super.priority,
                           &mca_errmgr_crmig_component.super.priority);
    
    mca_base_param_reg_int(&mca_errmgr_crmig_component.super.base_version,
                           "verbose",
                           "Verbose level for the ERRMGR crmig component",
                           false, false,
                           mca_errmgr_crmig_component.super.verbose, 
                           &mca_errmgr_crmig_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_errmgr_crmig_component.super.verbose) {
        mca_errmgr_crmig_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_errmgr_crmig_component.super.output_handle,
                                  mca_errmgr_crmig_component.super.verbose);
    } else {
        mca_errmgr_crmig_component.super.output_handle = orte_errmgr_base.output;
    }

    mca_base_param_reg_int(&mca_errmgr_crmig_component.super.base_version,
                           "timing",
                           "Enable Process Migration timer",
                           false, false,
                           0, &val);
    mca_errmgr_crmig_component.timing_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int(&mca_errmgr_crmig_component.super.base_version,
                           "enable",
                           "Enable Process Migration (Default: 0/off)",
                           false, false,
                           0, &val);
    mca_errmgr_crmig_component.crmig_enabled = OPAL_INT_TO_BOOL(val);

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: open()");
    opal_output_verbose(20, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: open: priority    = %d", 
                        mca_errmgr_crmig_component.super.priority);
    opal_output_verbose(20, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: open: verbosity   = %d", 
                        mca_errmgr_crmig_component.super.verbose);
    opal_output_verbose(20, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: open: Proc. Mig.  = %s", 
                        (mca_errmgr_crmig_component.crmig_enabled ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: open: timing      = %s", 
                        (mca_errmgr_crmig_component.timing_enabled ? "Enabled" : "Disabled"));

    return ORTE_SUCCESS;
}

static int errmgr_crmig_close(void)
{
    opal_output_verbose(10, mca_errmgr_crmig_component.super.output_handle,
                        "errmgr:crmig: close()");

    return ORTE_SUCCESS;
}
