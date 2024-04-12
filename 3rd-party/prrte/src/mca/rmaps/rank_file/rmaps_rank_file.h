/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Voltaire. All rights reserved
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Resource Mapping
 */

#ifndef PRTE_RMAPS_RF_H
#define PRTE_RMAPS_RF_H

#include "prte_config.h"

#include "src/class/pmix_object.h"

#include "src/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

#define RMAPS_RANK_FILE_MAX_SLOTS 64

int prte_rmaps_rank_file_lex_destroy(void);

struct prte_rmaps_rf_component_t {
    prte_rmaps_base_component_t super;
    char *slot_list;
};
typedef struct prte_rmaps_rf_component_t prte_rmaps_rf_component_t;

PRTE_MODULE_EXPORT extern prte_rmaps_rf_component_t prte_mca_rmaps_rank_file_component;
extern prte_rmaps_base_module_t prte_rmaps_rank_file_module;

typedef struct cpu_package_t cpu_package_t;

struct prte_rmaps_rank_file_map_t {
    pmix_object_t super;
    char *node_name;
    char slot_list[RMAPS_RANK_FILE_MAX_SLOTS];
};
typedef struct prte_rmaps_rank_file_map_t prte_rmaps_rank_file_map_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_rmaps_rank_file_map_t);

END_C_DECLS

#endif
