/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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

#include "opal/util/output.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#include "crcp_bkmrk.h"

/*
 * Public string for version number
 */
const char *ompi_crcp_bkmrk_component_version_string = 
"OMPI CRCP bkmrk MCA component version " OMPI_VERSION;

bool timing_enabled;

/*
 * Local functionality
 */
static int crcp_bkmrk_register(void);
static int crcp_bkmrk_open(void);
static int crcp_bkmrk_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
ompi_crcp_bkmrk_component_t mca_crcp_bkmrk_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component
         */
        {
            OMPI_CRCP_BASE_VERSION_2_0_0,
            /* Component name and version */
            "bkmrk",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,
            
            /* Component open and close functions */
            crcp_bkmrk_open,
            crcp_bkmrk_close,
            ompi_crcp_bkmrk_component_query,
            crcp_bkmrk_register
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
        20
    }
};

static int crcp_bkmrk_register(void)
{
    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_crcp_bkmrk_component.super.priority = 20;
    (void) mca_base_component_var_register(&mca_crcp_bkmrk_component.super.base_version,
                                           "priority",
                                           "Priority of the CRCP bkmrk component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_crcp_bkmrk_component.super.priority);

    mca_crcp_bkmrk_component.super.verbose = 0;
    (void) mca_base_component_var_register(&mca_crcp_bkmrk_component.super.base_version,
                                           "verbose",
                                           "Verbose level for the CRCP bkmrk component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_crcp_bkmrk_component.super.verbose);

    timing_enabled = false;
    (void) mca_base_component_var_register(&mca_crcp_bkmrk_component.super.base_version,
                                           "timing", "Enable Performance timing",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &timing_enabled);

    return OMPI_SUCCESS;
}

static int crcp_bkmrk_open(void)
{ 
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_crcp_bkmrk_component.super.verbose) {
        mca_crcp_bkmrk_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crcp_bkmrk_component.super.output_handle,
                                  mca_crcp_bkmrk_component.super.verbose);
    } else {
        mca_crcp_bkmrk_component.super.output_handle = ompi_crcp_base_framework.framework_output;
    }

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open()");
    opal_output_verbose(20, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open: priority   = %d", 
                        mca_crcp_bkmrk_component.super.priority);
    opal_output_verbose(20, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open: verbosity  = %d", 
                        mca_crcp_bkmrk_component.super.verbose);

    return OMPI_SUCCESS;
}

static int crcp_bkmrk_close(void)
{
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: close()");

    return OMPI_SUCCESS;
}
