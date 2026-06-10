/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_hwpc_cxi.h"

static int ompi_hook_hwpc_cxi_component_open(void);
static int ompi_hook_hwpc_cxi_component_close(void);
static int ompi_hook_hwpc_cxi_component_register(void);

const char *mca_hook_hwpc_cxi_component_version_string =
    "Open MPI 'hwpc_cxi' hook MCA component version " OMPI_VERSION;

const ompi_hook_base_component_1_0_0_t mca_hook_hwpc_cxi_component = {
    .hookm_version = {
        OMPI_HOOK_BASE_VERSION_1_0_0,

        .mca_component_name = "hwpc_cxi",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        .mca_open_component = ompi_hook_hwpc_cxi_component_open,
        .mca_close_component = ompi_hook_hwpc_cxi_component_close,
        .mca_register_component_params = ompi_hook_hwpc_cxi_component_register,

        .mca_component_flags = MCA_BASE_COMPONENT_FLAG_REQUIRED,
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
MCA_BASE_COMPONENT_INIT(ompi, hook, hwpc_cxi)

/* Global Component Variables */
char *mca_hook_hwpc_cxi_counter_file = NULL;
int mca_hook_hwpc_cxi_counter_report = 1;
char *mca_hook_hwpc_cxi_counter_report_file = NULL;
bool mca_hook_hwpc_cxi_counter_verbose = false;
bool mca_hook_hwpc_cxi_counter_summary_filter_zeros = true;

static int ompi_hook_hwpc_cxi_component_open(void)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}

static int ompi_hook_hwpc_cxi_component_close(void)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}

static int ompi_hook_hwpc_cxi_component_register(void)
{
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_file",
                                 "Specifies an absolute filepath to a file containing a comma-delimited list of hardware-based performance counters (HWPCs) and counter groups to enable for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_file);

    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_report",
                                 "An integer value between 0 and 5 to enable and control the level of reporting of HWPC counters for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_report);

    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_report_file",
                                 "Specifies an optional output filename prefix for the HWPC counter reports for HPE's Cassini devices to be written.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_report_file);

    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_verbose",
                                 "A boolean value indicates the verbosity output setting about the HWPC counters for HPE's Cassini devices that are being collected.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_verbose);

    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_summary_filter_zeros",
                                 "A boolean value for whether (true) or not (false) to filter out zero-valued deltas for HWPC counter summary reports for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_summary_filter_zeros);

    return OMPI_SUCCESS;
}