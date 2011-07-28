/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
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
#include "errmgr_hnpresil.h"

/*
 * Public string for version number
 */
const char *orte_errmgr_hnpresil_component_version_string = 
    "ORTE ERRMGR hnpresil MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int orte_errmgr_hnpresil_open(void);
static int orte_errmgr_hnpresil_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_errmgr_hnpresil_component_t mca_errmgr_hnpresil_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component hnp
         */
        {
            ORTE_ERRMGR_BASE_VERSION_3_0_0,
            /* Component name and version */
            "hnpresil",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
        
            /* Component open and close functions */
            orte_errmgr_hnpresil_open,
            orte_errmgr_hnpresil_close,
            orte_errmgr_hnpresil_component_query
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

static int orte_errmgr_hnpresil_open(void) 
{
    int val;

    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "priority",
                           "Priority of the ERRMGR hnp component",
                           false, false,
                           mca_errmgr_hnpresil_component.super.priority,
                           &mca_errmgr_hnpresil_component.super.priority);
    
    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "verbose",
                           "Verbose level for the ERRMGR hnp component",
                           false, false,
                           mca_errmgr_hnpresil_component.super.verbose, 
                           &mca_errmgr_hnpresil_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_errmgr_hnpresil_component.super.verbose) {
        mca_errmgr_hnpresil_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_errmgr_hnpresil_component.super.output_handle,
                                  mca_errmgr_hnpresil_component.super.verbose);
    } else {
        mca_errmgr_hnpresil_component.super.output_handle = orte_errmgr_base.output;
    }

#if OPAL_ENABLE_FT_CR
    /****************************
     * CRMig (C/R Process Migration) MCA Options
     ****************************/
    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "crmig_timing",
                           "Enable Process Migration timer",
                           false, false,
                           0, &val);
    mca_errmgr_hnpresil_component.crmig_timing_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "crmig_enable",
                           "Enable Process Migration (Default: 0/off)",
                           false, false,
                           0, &val);
    mca_errmgr_hnpresil_component.crmig_enabled = OPAL_INT_TO_BOOL(val);

    /****************************
     * AutoR (Automatic Recovery) MCA Options
     ****************************/
    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "autor_timing",
                           "Enable Automatic Recovery timer",
                           false, false,
                           0, &val);
    mca_errmgr_hnpresil_component.autor_timing_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "autor_enable",
                           "Enable Automatic Recovery (Default: 0/off)",
                           false, false,
                           0, &val);
    mca_errmgr_hnpresil_component.autor_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "autor_recovery_delay",
                           "Number of seconds to wait before starting to recover the job after a failure"
                           " [Default: 1 sec]",
                           false, false,
                           1, &val);
    mca_errmgr_hnpresil_component.autor_recovery_delay = val;

    mca_base_param_reg_int(&mca_errmgr_hnpresil_component.super.base_version,
                           "autor_skip_oldnode",
                           "Skip the old node from failed proc, even if it is still available"
                           " [Default: Enabled]",
                           false, false,
                           1, &val);
    mca_errmgr_hnpresil_component.autor_skip_oldnode = OPAL_INT_TO_BOOL(val);
#else
    val = 0; /* Silence compiler warning */
#endif /* OPAL_ENABLE_FT_CR */

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open()");
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: priority      = %d", 
                        mca_errmgr_hnpresil_component.super.priority);
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: verbosity     = %d", 
                        mca_errmgr_hnpresil_component.super.verbose);
#if OPAL_ENABLE_FT_CR
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open:  --- CR Migration Options   ---");
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: Process Migration = %s", 
                        (mca_errmgr_hnpresil_component.crmig_enabled ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: timing        = %s", 
                        (mca_errmgr_hnpresil_component.crmig_timing_enabled ? "Enabled" : "Disabled"));

    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open:  --- Auto. Recovery Options ---");
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: Auto. Recover = %s", 
                        (mca_errmgr_hnpresil_component.autor_enabled ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: timing        = %s", 
                        (mca_errmgr_hnpresil_component.autor_timing_enabled ? "Enabled" : "Disabled"));
    opal_output_verbose(20, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: open: recover_delay = %d",
                        mca_errmgr_hnpresil_component.autor_recovery_delay);

    mca_errmgr_hnpresil_component.crmig_in_progress = false;
    mca_errmgr_hnpresil_component.autor_in_progress = false;
    mca_errmgr_hnpresil_component.term_in_progress  = false;
#endif /* OPAL_ENABLE_FT_CR */

    return ORTE_SUCCESS;
}

static int orte_errmgr_hnpresil_close(void)
{
    opal_output_verbose(10, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp: close()");

    return ORTE_SUCCESS;
}
