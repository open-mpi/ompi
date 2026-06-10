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

#ifndef MCA_HOOK_HWPC_CXI_H
#define MCA_HOOK_HWPC_CXI_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/util/output.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

BEGIN_C_DECLS

OMPI_DECLSPEC extern const ompi_hook_base_component_1_0_0_t mca_hook_hwpc_cxi_component;

extern char *mca_hook_hwpc_cxi_counter_file;
extern int mca_hook_hwpc_cxi_counter_report;
extern bool mca_hook_hwpc_cxi_counter_verbose;
extern bool mca_hook_hwpc_cxi_counter_summary_filter_zeros;
extern char *mca_hook_hwpc_cxi_counter_report_file;

void ompi_hook_hwpc_cxi_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

void ompi_hook_hwpc_cxi_mpi_finalize_top(void);

END_C_DECLS

#endif /* MCA_HOOK_HWPC_CXI_H */

