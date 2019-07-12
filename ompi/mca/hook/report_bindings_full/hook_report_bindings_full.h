/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_HOOK_REPORT_BINDINGS_FULL_H
#define MCA_HOOK_REPORT_BINDINGS_FULL_H

#include "ompi_config.h"

#include "ompi/constants.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

BEGIN_C_DECLS

OMPI_MODULE_DECLSPEC extern const ompi_hook_base_component_1_0_0_t mca_hook_report_bindings_full_component;

extern int mca_hook_report_bindings_full_verbose;
extern int mca_hook_report_bindings_full_output;
extern bool hook_report_bindings_full_enable_mpi_init;

void ompi_hook_report_bindings_full_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

END_C_DECLS

#endif /* MCA_HOOK_REPORT_BINDINGS_FULL_H */
