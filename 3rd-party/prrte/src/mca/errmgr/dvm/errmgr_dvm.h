/*
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 *
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

#ifndef prte_errmgr_dvm_EXPORT_H
#define prte_errmgr_dvm_EXPORT_H

#include "prte_config.h"

#include "src/mca/errmgr/errmgr.h"

BEGIN_C_DECLS

/*
 * Local Component structures
 */

PRTE_MODULE_EXPORT extern prte_errmgr_base_component_t prte_mca_errmgr_dvm_component;

PRTE_EXPORT extern prte_errmgr_base_module_t prte_errmgr_dvm_module;

END_C_DECLS

#endif /* prte_errmgr_dvm_EXPORT_H */
