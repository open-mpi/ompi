/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PRM_PBS_H
#define PMIX_PRM_PBS_H

#include "src/include/pmix_config.h"

#include "src/mca/prm/prm.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_prm_base_component_t pmix_mca_prm_pbs_component;
extern pmix_prm_module_t pmix_prm_pbs_module;

END_C_DECLS

#endif
