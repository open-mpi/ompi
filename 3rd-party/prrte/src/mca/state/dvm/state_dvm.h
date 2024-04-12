/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#ifndef MCA_STATE_DVM_EXPORT_H
#define MCA_STATE_DVM_EXPORT_H

#include "prte_config.h"

#include "src/mca/state/state.h"

BEGIN_C_DECLS

/*
 * Local Component structures
 */

PRTE_MODULE_EXPORT extern prte_state_base_component_t prte_mca_state_dvm_component;

PRTE_EXPORT extern prte_state_base_module_t prte_state_dvm_module;

END_C_DECLS

#endif /* MCA_STATE_DVM_EXPORT_H */
