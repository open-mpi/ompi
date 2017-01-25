/*
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_HOOK_PROT_H
#define MCA_HOOK_PROT_H

#include "ompi_config.h"

#include "ompi/constants.h"

#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"

BEGIN_C_DECLS

OMPI_MODULE_DECLSPEC extern const ompi_hook_base_component_2_0_0_t mca_hook_prot_component;

extern int mca_hook_prot_verbose;
extern int mca_hook_prot_output;
extern bool hook_prot_enable_mpi_init;
extern bool hook_prot_enable_mpi_finalize;

void ompi_hook_prot_mpi_init_bottom(int argc, char **argv, int requested, int *provided);

void ompi_hook_prot_mpi_finalize_top(void);

END_C_DECLS

#endif /* MCA_HOOK_PROT_H */
