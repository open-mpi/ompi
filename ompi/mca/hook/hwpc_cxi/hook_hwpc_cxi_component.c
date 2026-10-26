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
#include "ompi/runtime/ompi_rte.h"

static int ompi_hook_hwpc_cxi_component_open(void);
static int ompi_hook_hwpc_cxi_component_close(void);
static int ompi_hook_hwpc_cxi_component_register(void);
#if !OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI
static void ompi_hook_hwpc_cxi_mpi_init_bottom_noop(int argc, char **argv, int requested, int *provided);
#endif

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
#if OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI
    .hookm_mpi_init_bottom = ompi_hook_hwpc_cxi_mpi_init_bottom,
#else
    .hookm_mpi_init_bottom = ompi_hook_hwpc_cxi_mpi_init_bottom_noop,
#endif
    .hookm_mpi_init_error = NULL,

#if OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI
    .hookm_mpi_finalize_top = ompi_hook_hwpc_cxi_mpi_finalize_top,
#else
    .hookm_mpi_finalize_top = NULL,
#endif
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

static int ompi_hook_hwpc_cxi_var_indices[6];
static size_t ompi_hook_hwpc_cxi_num_vars = 0;

static const mca_base_var_enum_value_t ompi_hook_hwpc_cxi_counter_report_levels[] = {
    {.value = 0, .string = "quiet"},
    {.value = 1, .string = "timeouts"},
    {.value = 2, .string = "summary"},
    {.value = 3, .string = "timeout-details"},
    {.value = 4, .string = "details"},
    {.value = 5, .string = "details-and-descriptions"},
    {.string = NULL},
};

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
    mca_base_var_enum_t *counter_report_enum;
    int ret;

    ompi_hook_hwpc_cxi_num_vars = 0;
    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] =
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_file",
                                 "Specifies an absolute filepath to a file containing a comma-delimited list of hardware-based performance counters (HWPCs) and counter groups to enable for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_file);

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
    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] = ret;

    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] =
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_report_file",
                                 "Specifies an optional output filename prefix for the HWPC counter reports for HPE's Cassini devices to be written.",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_report_file);

    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] =
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_verbose",
                                 "A boolean value indicates the verbosity output setting about the HWPC counters for HPE's Cassini devices that are being collected.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_verbose);

    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] =
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_summary_filter_zeros",
                                 "A boolean value for whether (true) or not (false) to filter out zero-valued deltas for HWPC counter summary reports for HPE's Cassini devices.",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_summary_filter_zeros);

    ompi_hook_hwpc_cxi_var_indices[ompi_hook_hwpc_cxi_num_vars++] =
    mca_base_component_var_register(&mca_hook_hwpc_cxi_component.hookm_version, "counter_mpi_t_enable",
                                 "A boolean value for whether to register HWPC counters for HPE's Cassini devices as MPI_T performance variables (default: false).",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_4,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_hook_hwpc_cxi_counter_mpi_t_enable);

    return OMPI_SUCCESS;
}

#if !OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI
static void ompi_hook_hwpc_cxi_mpi_init_bottom_noop(int argc, char **argv, int requested, int *provided)
{
    static bool warned = false;
    mca_base_var_source_t source;

    if (warned || 0 == mca_hook_hwpc_cxi_counter_report || 0 != OMPI_PROC_MY_NAME->vpid) {
        return;
    }

    for (size_t i = 0; i < ompi_hook_hwpc_cxi_num_vars; ++i) {
        if (0 <= ompi_hook_hwpc_cxi_var_indices[i]
            && OPAL_SUCCESS == mca_base_var_get_value(ompi_hook_hwpc_cxi_var_indices[i], NULL, &source, NULL)
            && MCA_BASE_VAR_SOURCE_DEFAULT != source) {
            warned = true;
            opal_output(0, "HWPC_CXI: WARNING: Open MPI was built without libcxi/libcxi.h; "
                           "hwpc_cxi MCA parameters are ignored and no Cassini counters will be collected");
            return;
        }
    }
}
#endif