/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c)      2010-2011 Alex Brick <bricka@ccs.neu.edu>.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/output.h"

#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "crs_dmtcp.h"

/*
 * Local functionality
 */
static int crs_dmtcp_register (void);
static int crs_dmtcp_open(void);
static int crs_dmtcp_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
opal_crs_dmtcp_component_t mca_crs_dmtcp_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itself
         */
        {
            OPAL_CRS_BASE_VERSION_2_0_0,

            /* Component name and version */
            "dmtcp",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            crs_dmtcp_open,
            crs_dmtcp_close,
            opal_crs_dmtcp_component_query,
            crs_dmtcp_register
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

static int crs_dmtcp_register (void)
{
    int ret;
    /*
     * User can adjust the relative priority of this component with respect
     * to other CRS components available for selection.
     */
    mca_crs_dmtcp_component.super.priority = 20
    ret = mca_base_component_var_register (&mca_crs_dmtcp_component.super.base_version,
                                           "priority", "Priority of the CRS dmtcp component "
                                           "(default: 20)", MCA_BASE_VAR_TYPE_INT, NULL,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_crs_dmtcp_component.super.priority);
    if (0 > ret) {
        return ret;
    }

    /*
     * Adjust the verbosity level for this component. Default off or 0.
     */
    mca_crs_dmtcp_component.super.verbose = 0;
    ret = mca_base_component_var_register (&mca_crs_dmtcp_component.super.base_version,
                                           "verbose",
                                           "Verbose level for the CRS dmtcp component",
                                           MCA_BASE_VAR_TYPE_INT, NULL,MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_crs_dmtcp_component.super.verbose);
    return (0 > ret) ? ret : OPAL_SUCCESS;
}
 
static int crs_dmtcp_open(void) 
{
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_crs_dmtcp_component.super.verbose) {
        mca_crs_dmtcp_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crs_dmtcp_component.super.output_handle, 
                                  mca_crs_dmtcp_component.super.verbose);
    } else {
        mca_crs_dmtcp_component.super.output_handle = opal_crs_base_framework.framework_output;
    }

    /*
     * Debug output
     */
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: open()");
    opal_output_verbose(20, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: open: priority = %d", 
                        mca_crs_dmtcp_component.super.priority);
    opal_output_verbose(20, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: open: verbosity = %d", 
                        mca_crs_dmtcp_component.super.verbose);

    return OPAL_SUCCESS;
}

static int crs_dmtcp_close(void)
{
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: close()");

    return OPAL_SUCCESS;
}
