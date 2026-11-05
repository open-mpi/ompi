/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_hwpc_cxi.h"

#include "opal/mca/base/mca_base_var.h"
#include "opal/util/show_help.h"

static int ompi_hook_hwpc_cxi_component_open(void);
static int ompi_hook_hwpc_cxi_component_close(void);
static int ompi_hook_hwpc_cxi_component_register(void);

OMPI_HIDDEN const char *mca_hook_hwpc_cxi_component_version_string =
    "Open MPI 'hwpc_cxi' hook MCA component version " OMPI_VERSION;

OMPI_HIDDEN const ompi_hook_base_component_1_0_0_t mca_hook_hwpc_cxi_component = {
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,

        .mca_component_name = "hwpc_cxi",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        .mca_open_component = ompi_hook_hwpc_cxi_component_open,
        .mca_close_component = ompi_hook_hwpc_cxi_component_close,
        .mca_register_component_params = ompi_hook_hwpc_cxi_component_register,
    },
    .hookm_data = {
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Component functions */
    .hookm_mpi_initialized_top = NULL,
    .hookm_mpi_initialized_bottom = NULL,

    .hookm_mpi_finalized_top = NULL,
    .hookm_mpi_finalized_bottom = NULL,

    .hookm_mpi_init_top = NULL,
    .hookm_mpi_init_top_post_opal = NULL,
    .hookm_mpi_init_bottom = ompi_hook_hwpc_cxi_mpi_init_bottom,
    .hookm_mpi_init_error = NULL,

    .hookm_mpi_finalize_top = ompi_hook_hwpc_cxi_mpi_finalize_top,
    .hookm_mpi_finalize_bottom = NULL,
};
OMPI_HIDDEN const mca_base_component_t *mca_hook_hwpc_cxi_component_ptr =
    (const mca_base_component_t *)&mca_hook_hwpc_cxi_component;

/* Global Component Variables */
char *mca_hook_hwpc_cxi_counter_file = NULL;
int mca_hook_hwpc_cxi_counter_report = 0;
char *mca_hook_hwpc_cxi_counter_report_file = NULL;
bool mca_hook_hwpc_cxi_counter_verbose = false;
bool mca_hook_hwpc_cxi_counter_summary_filter_zeros = true;
bool mca_hook_hwpc_cxi_counter_mpi_t_enable = false;

static const mca_base_var_enum_value_t ompi_hook_hwpc_cxi_counter_report_levels[] = {
    {.value = 0, .string = "quiet"},
    {.value = 1, .string = "timeouts"},
    {.value = 2, .string = "summary"},
    {.value = 3, .string = "timeout-nics"},
    {.value = 4, .string = "all-nics-on-timeout"},
    {.value = 5, .string = "all-nics"},
    {.string = NULL},
};

static int ompi_hook_hwpc_cxi_component_open(void)
{
    /* At level 0 (the default) the hook framework drops this component, so none of its hooks run. */
    if (0 == mca_hook_hwpc_cxi_counter_report) {
        if (mca_hook_hwpc_cxi_counter_mpi_t_enable) {
            opal_show_help("help-hook-hwpc-cxi.txt", "mpi-t-needs-report-level", true);
        }
        return OPAL_ERR_NOT_AVAILABLE;
    }
    return OMPI_SUCCESS;
}

static int ompi_hook_hwpc_cxi_component_close(void)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}

static int ompi_hook_hwpc_cxi_component_register(void)
{
    mca_base_var_enum_t *counter_report_enum;
    int ret;

    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_file",
                                 "Path to a file listing Cassini counter names, counter mnemonics, or counter groups (separated by commas, spaces, tabs, or newlines) to collect instead of the default list.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_file);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_enum_create("ompi_hook_hwpc_cxi_counter_report",
                                   ompi_hook_hwpc_cxi_counter_report_levels,
                                   &counter_report_enum);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }
    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_report",
                                          "Reporting level for HWPC counters on HPE Cassini devices (default: 0, quiet: nothing is collected).",
                                          MCA_BASE_VAR_TYPE_INT, counter_report_enum, 0, 0,
                                          OPAL_INFO_LVL_4,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_hook_hwpc_cxi_counter_report);
    OBJ_RELEASE(counter_report_enum);
    if (0 > ret) {
        return ret;
    }

    mca_hook_hwpc_cxi_counter_report_file = HWPC_CXI_DEFAULT_REPORT_FILE_PREFIX;
    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_report_file",
                                 "Filename prefix for the per-host detail reports written at report levels 3-5; each node writes <prefix>.<hostname>, relative to the node leader's working directory unless the prefix is an absolute path.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_report_file);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_verbose",
                                 "Print diagnostic messages about the Cassini counters being selected and collected (default: false).",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_verbose);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_summary_filter_zeros",
                                 "A boolean value for whether (true) or not (false) to filter out zero-valued deltas for HWPC counter summary reports for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_summary_filter_zeros);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_mpi_t_enable",
                                 "Register the collected Cassini counters as MPI_T performance variables (default: false). Requires hook_hwpc_cxi_counter_report of 1 or higher; at level 0 nothing is registered.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_mpi_t_enable);
    if (0 > ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}
