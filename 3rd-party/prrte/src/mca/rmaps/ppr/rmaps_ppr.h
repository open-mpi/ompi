/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_RMAPS_PPR_H
#define PRTE_RMAPS_PPR_H

#include "prte_config.h"

#include "src/hwloc/hwloc-internal.h"

#include "src/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

PRTE_MODULE_EXPORT extern prte_rmaps_base_component_t prte_mca_rmaps_ppr_component;
extern prte_rmaps_base_module_t prte_rmaps_ppr_module;

END_C_DECLS

#endif
