/*
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_HOOK_DEMO_H
#define MCA_HOOK_DEMO_H

#include "ompi_config.h"

#include "ompi/constants.h"

#include "opal/util/output.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

BEGIN_C_DECLS

OMPI_MODULE_DECLSPEC extern const ompi_hook_base_component_1_0_0_t mca_hook_demo_component;

void ompi_hook_demo_mpi_initialized_top(int *flag);
void ompi_hook_demo_mpi_initialized_bottom(int *flag);

void ompi_hook_demo_mpi_finalized_top(int *flag);
void ompi_hook_demo_mpi_finalized_bottom(int *flag);

void ompi_hook_demo_mpi_init_top(int argc, char **argv, int requested, int *provided);
void ompi_hook_demo_mpi_init_top_post_opal(int argc, char **argv, int requested, int *provided);
void ompi_hook_demo_mpi_init_bottom(int argc, char **argv, int requested, int *provided);
void ompi_hook_demo_mpi_init_error(int argc, char **argv, int requested, int *provided);

void ompi_hook_demo_mpi_finalize_top(void);
void ompi_hook_demo_mpi_finalize_bottom(void);

void ompi_hook_demo_extra_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

END_C_DECLS

#endif /* MCA_HOOK_DEMO_H */
