/*
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PRM_SLURM_H_
#define PMIX_PRM_SLURM_H_

#include "pmix_config.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/prm/prm.h"


BEGIN_C_DECLS

PMIX_EXPORT extern pmix_prm_base_component_t pmix_mca_prm_slurm_component;
extern pmix_prm_module_t pmix_prm_slurm_module;

END_C_DECLS

#endif /* PMIX_PRM_SLURM_H_ */

