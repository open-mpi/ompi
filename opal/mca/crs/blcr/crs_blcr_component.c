/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
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

#include "opal/util/output.h"

#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "crs_blcr.h"

/*
 * Local functionality
 */
static int crs_blcr_register (void);
static int crs_blcr_open(void);
static int crs_blcr_close(void);

bool opal_crs_blcr_dev_null = false;

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
opal_crs_blcr_component_t mca_crs_blcr_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itself
         */
        {
            OPAL_CRS_BASE_VERSION_2_0_0,

            /* Component name and version */
            "blcr",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            crs_blcr_open,
            crs_blcr_close,
            opal_crs_blcr_component_query,
            crs_blcr_register
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

static int crs_blcr_register (void) 
{
    int ret;

    mca_crs_blcr_component.super.priority = 10;
    ret = mca_base_component_var_register (&mca_crs_blcr_component.super.base_version,
                                           "priority", "Priority of the CRS blcr component "
                                           "(default: 10)". MCA_BASE_VAR_TYPE_INT, NULL,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_crs_blcr_component.super.priority);
    if (0 > ret) {
        return ret;
    }

    mca_crs_blcr_component.super.verbose = 0;
    ret = mca_base_component_var_register (&mca_crs_blcr_component.super.base_version,
                                           "verbose",
                                           "Verbose level for the CRS blcr component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_crs_blcr_component.super.verbose);
    if (0 > ret) {
        return ret;
    }

    opal_crs_blcr_dev_null = false;
    ret = mca_base_component_var_register (&mca_crs_blcr_component.super.base_version,
                                           "dev_null",
                                           "Not for general use! For debugging only! Save checkpoint to /dev/null. [Default = disabled]",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &opal_crs_blcr_dev_null);
    return (0 > ret) ? ret : OPAL_SUCCESS
}

static int crs_blcr_open(void) 
{
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_crs_blcr_component.super.verbose) {
        mca_crs_blcr_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crs_blcr_component.super.output_handle, 
                                  mca_crs_blcr_component.super.verbose);
    } else {
        mca_crs_blcr_component.super.output_handle = opal_crs_base_framework.framework_output;
    }

    /*
     * Debug output
     */
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: open()");
    opal_output_verbose(20, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: open: priority = %d", 
                        mca_crs_blcr_component.super.priority);
    opal_output_verbose(20, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: open: verbosity = %d", 
                        mca_crs_blcr_component.super.verbose);
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: open: dev_null = %s",
                        (opal_crs_blcr_dev_null == true ? "True" : "False"));

    return OPAL_SUCCESS;
}

static int crs_blcr_close(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: close()");

    return OPAL_SUCCESS;
}
