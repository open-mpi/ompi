/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "crs_criu.h"

/* Local functionality */
static int crs_criu_register(void);
static int crs_criu_open(void);
static int crs_criu_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
opal_crs_criu_component_t mca_crs_criu_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing
         *  meta information about the component itself
         */
        .base_version = {
            OPAL_CRS_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "criu",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = crs_criu_open,
            .mca_close_component = crs_criu_close,
            .mca_query_component = opal_crs_criu_component_query,
            .mca_register_component_params = crs_criu_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .verbose = 0,
        .output_handle = -1,
    },
    /* criu log file */
    LOG_FILE,
    /* criu log level */
    0,
    /* criu tcp established */
    true,
    /* criu shell job */
    true,
    /* criu external unix sockets */
    true,
    /* criu leave tasks in running state after checkpoint */
    true
};

static int crs_criu_register(void)
{
    int ret;

    mca_base_component_t *component = &mca_crs_criu_component.super.base_version;

    mca_crs_criu_component.super.priority = 10;
    ret = mca_base_component_var_register(component, "priority",
                                          "Priority of the CRS criu component (default: 10)",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL_EQ,
                                          &mca_crs_criu_component.super.priority);
    if (0 > ret) {
        return ret;
    }

    mca_crs_criu_component.super.verbose = 0;
    ret = mca_base_component_var_register(component, "verbose",
                                          "Verbose level for the CRS criu component",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.super.verbose);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "log", "Name of CRIU logfile (default: criu.log)",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.log_file);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "log_level",
                                          "Verbose level for the CRS criu component (default: 0)",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.log_level);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "tcp_established",
                                          "Checkpoint/restore established TCP connections (default: true)",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.tcp_established);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "shell_job",
                                          "Allow to dump and restore shell jobs (default: true)",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.shell_job);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "ext_unix_sk",
                                          "Allow external unix connections (default: true)",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.ext_unix_sk);

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(component, "leave_running",
                                          "Leave tasks in running state after checkpoint (default: true)",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_crs_criu_component.leave_running);

    return (0 > ret) ? ret : OPAL_SUCCESS;
}

static int crs_criu_open(void)
{
    int oh;

    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if (0 != mca_crs_criu_component.super.verbose) {
        mca_crs_criu_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crs_criu_component.super.output_handle,
                                  mca_crs_criu_component.super.verbose);
    } else {
        mca_crs_criu_component.super.output_handle = opal_crs_base_framework.framework_output;
    }

    oh = mca_crs_criu_component.super.output_handle;
    /*
     * Debug output
     */
    opal_output_verbose(10, oh, "crs:criu: open()");
    opal_output_verbose(20, oh, "crs:criu: open: priority = %d",
                        mca_crs_criu_component.super.priority);
    opal_output_verbose(20, oh, "crs:criu: open: verbosity = %d",
                        mca_crs_criu_component.super.verbose);
    opal_output_verbose(20, oh, "crs:criu: open: log_file = %s",
                        mca_crs_criu_component.log_file);
    opal_output_verbose(20, oh, "crs:criu: open: log_level = %d",
                        mca_crs_criu_component.log_level);
    opal_output_verbose(20, oh, "crs:criu: open: tcp_established = %d",
                        mca_crs_criu_component.tcp_established);
    opal_output_verbose(20, oh, "crs:criu: open: shell_job = %d",
                        mca_crs_criu_component.shell_job);
    opal_output_verbose(20, oh, "crs:criu: open: ext_unix_sk = %d",
                        mca_crs_criu_component.ext_unix_sk);
    opal_output_verbose(20, oh, "crs:criu: open: leave_running = %d",
                        mca_crs_criu_component.leave_running);

    return OPAL_SUCCESS;
}

static int crs_criu_close(void)
{
    opal_output_verbose(10, mca_crs_criu_component.super.output_handle,
                        "crs:criu: close()");

    return OPAL_SUCCESS;
}
