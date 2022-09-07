/*
 * Copyright (c) 2016-2022 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_HOOK_COMM_METHOD_H
#define MCA_HOOK_COMM_METHOD_H

#include "ompi_config.h"

#include "ompi/constants.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

BEGIN_C_DECLS

OMPI_DECLSPEC extern ompi_hook_base_component_1_0_0_t mca_hook_comm_method_component;

extern int mca_hook_comm_method_verbose;
extern int mca_hook_comm_method_output;
extern bool mca_hook_comm_method_enable_mpi_init;
extern bool mca_hook_comm_method_enable_mpi_finalize;
extern int mca_hook_comm_method_max;
extern bool mca_hook_comm_method_brief;
extern char *mca_hook_comm_method_fakefile;

void ompi_hook_comm_method_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

void ompi_hook_comm_method_mpi_finalize_top(void);

END_C_DECLS

#endif /* MCA_HOOK_COMM_METHOD_H */
