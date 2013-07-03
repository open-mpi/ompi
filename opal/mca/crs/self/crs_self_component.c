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

#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/util/output.h"
#include "crs_self.h"

/*
 * Public string for version number
 */
const char *opal_crs_self_component_version_string = 
"OPAL CRS self MCA component version " OPAL_VERSION;

/*
 * Local functionality
 */
static int crs_self_register (void);
static int crs_self_open(void);
static int crs_self_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
opal_crs_self_component_t mca_crs_self_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itself
         */
        {
            OPAL_CRS_BASE_VERSION_2_0_0,

            /* Component name and version */
            "self",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            crs_self_open,
            crs_self_close,
            opal_crs_self_component_query,
            crs_self_register
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
    /* Default prefix */
    PREFIX_DEFAULT,
    /* If we are restarting right out of the gate */
    false,
    /* Checkpointing enabled */
    true,
    /* Callbacks */
    NULL,
    NULL,
    NULL
};

static int crs_self_register (void)
{
    int ret;

    /* Default priority */
    mca_crs_self_component.super.priority = 20;
    ret = mca_base_component_var_register (&mca_crs_self_component.super.base_version,
                                           "priority", "Priority of the CRS self component "
                                           "(default: 20)", MCA_BASE_VAR_TYPE_INT, NULL,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MPI_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_crs_self_component.super.priority);
    if (0 > ret) {
        return ret;
    }

    mca_crs_self_component.super.verbose = 0;
    ret = mca_base_component_var_register (&mca_crs_self_component.super.base_version,
                                           "verbose",
                                           "Verbose level for the CRS self component",
                                           MCA_BASE_VAR_TYPE_INT, NULL,MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MPI_BASE_VAR_SCOPE_LOCAL,
                                           &mca_crs_self_component.super.verbose);
    if (0 > ret) {
        return ret;
    }

    /*
     * Handler names
     */
    mca_crs_self_component.prefix = NULL;
    ret = mca_base_component_var_register (&mca_crs_self_component.super.base_version,
                                           "prefix",
                                           "Prefix for user defined callback functions",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MPI_BASE_VAR_SCOPE_LOCAL,
                                           &mca_crs_self_component.prefix);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register (&mca_crs_self_component.super.base_version,
                                           "do_restart",
                                           "Start execution by calling restart callback",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MPI_BASE_VAR_SCOPE_LOCAL,
                                           &mca_crs_self_component.do_restart);
    return (0 > ret) ? ret : OPAL_SUCCESS;
}
 
static int crs_self_open(void) 
{
    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */

    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_crs_self_component.super.verbose) {
        mca_crs_self_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crs_self_component.super.output_handle,
                                  mca_crs_self_component.super.verbose);
    } else {
        mca_crs_self_component.super.output_handle = opal_crs_base_framework.framework_output;
    }
    
    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: open()");
    opal_output_verbose(20, mca_crs_self_component.super.output_handle,
                        "crs:self: open: priority   = %d", 
                        mca_crs_self_component.super.priority);
    opal_output_verbose(20, mca_crs_self_component.super.output_handle,
                        "crs:self: open: verbosity  = %d", 
                        mca_crs_self_component.super.verbose);
    opal_output_verbose(20, mca_crs_self_component.super.output_handle,
                        "crs:self: open: prefix     = %s", 
                        mca_crs_self_component.prefix);
    opal_output_verbose(20, mca_crs_self_component.super.output_handle,
                        "crs:self: open: do_restart = %d", 
                        mca_crs_self_component.do_restart);

    return OPAL_SUCCESS;
}

static int crs_self_close(void)
{
    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: close()");

    return OPAL_SUCCESS;
}
