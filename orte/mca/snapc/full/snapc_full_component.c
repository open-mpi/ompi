/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
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
#include "opal/util/output.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "snapc_full.h"

/*
 * Public string for version number
 */
const char *orte_snapc_full_component_version_string = 
"ORTE SNAPC full MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int snapc_full_open(void);
static int snapc_full_close(void);

bool orte_snapc_full_skip_app   = false;
bool orte_snapc_full_timing_enabled = false;
int orte_snapc_full_progress_meter = 0;
int orte_snapc_full_max_wait_time = 20;

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_snapc_full_component_t mca_snapc_full_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itfull
         */
        {
            ORTE_SNAPC_BASE_VERSION_2_0_0,
            /* Component name and version */
            "full",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            snapc_full_open,
            snapc_full_close,
            orte_snapc_full_component_query,
            snapc_full_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Verbosity level */
        0,
        /* opal_output handler */
        -1
    }
};

static int snaps_full_register (void)
{
    mca_base_component_t *component = &mca_snapc_full_component.super.base_version;
    /*
     * This should be the last component to ever get used since
     * it doesn't do anything.
      */

    /* Default priority */
    mca_snapc_full_component.super.priority = 20;
    (void) mca_base_component_var_register (component, "priority", "Priority of the SNAPC full component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_snapc_full_component.super.priority);

    mca_snapc_full_component.super.verbose = 0;
    (void) mca_base_component_var_register (component, "verbose",
                                            "Verbose level for the SNAPC full component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL,
                                            &mca_snapc_full_component.super.verbose);

    orte_snapc_full_skip_app = false;
    (void) mca_base_component_var_register (component, "skip_app",
                                            "Not for general use! For debugging only! Shortcut app level coord. [Default = disabled]",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL,
                                            &orte_snapc_full_skip_app);

    orte_snapc_full_timing_enabled = false;
    (void) mca_base_component_var_register (component, "enable_timing",
                                            "Enable timing information. [Default = disabled]",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL,
                                            &orte_snapc_full_timing_enabled);

    orte_snapc_full_max_wait_time = 20;
    (void) mca_base_component_var_register (component, "max_wait_time",
                                            "Wait time before orted gives up on checkpoint (seconds)",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL,
                                            &orte_snapc_full_max_wait_time);

    orte_snapc_full_progress_meter = 0;
    (void) mca_base_component_var_register (component, "progress_meter",
                                            "Display Progress every X percentage done. [Default = 0/off]",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL,
                                            &orte_snapc_full_progress_meter);
    orte_snapc_full_progress_meter = (value % 101);

    return ORTE_SUCCESS;
}

static int snapc_full_open(void) 
{
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_snapc_full_component.super.verbose) {
        mca_snapc_full_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_snapc_full_component.super.output_handle,
                                  mca_snapc_full_component.super.verbose);
    } else {
        mca_snapc_full_component.super.output_handle = orte_snapc_base_framework.framework_output;
    }

    /* recheck the progress meter (it may have changed between register and open) */
    orte_snapc_full_progress_meter = (value % 101);

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open()");
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: priority    = %d", 
                        mca_snapc_full_component.super.priority);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: verbosity   = %d", 
                        mca_snapc_full_component.super.verbose);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: max_wait_time  = %d", 
                        orte_snapc_full_max_wait_time);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: progress_meter = %d", 
                        orte_snapc_full_progress_meter);

    return ORTE_SUCCESS;
}

static int snapc_full_close(void)
{
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: close()");

    return ORTE_SUCCESS;
}
