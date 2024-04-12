/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * pstat (process statistics) framework component interface.
 *
 * Intent
 *
 * To support the ompi-top utility.
 *
 */

#ifndef PMIX_MCA_PSTAT_H
#define PMIX_MCA_PSTAT_H

#include "pmix_config.h"
#include "pmix_common.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

BEGIN_C_DECLS

/**
 * Module initialization function.  Should return PMIX_SUCCESS.
 */
typedef int (*pmix_pstat_base_module_init_fn_t)(void);

typedef int (*pmix_pstat_base_module_query_fn_t)(pid_t pid, pmix_proc_stats_t *stats,
                                                 pmix_node_stats_t *nstats);

typedef int (*pmix_pstat_base_module_fini_fn_t)(void);

/**
 * Structure for pstat components.
 */
typedef pmix_mca_base_component_t pmix_pstat_base_component_t;

/**
 * Structure for pstat modules
 */
struct pmix_pstat_base_module_1_0_0_t {
    pmix_pstat_base_module_init_fn_t init;
    pmix_pstat_base_module_query_fn_t query;
    pmix_pstat_base_module_fini_fn_t finalize;
};

/**
 * Convenience typedef
 */
typedef struct pmix_pstat_base_module_1_0_0_t pmix_pstat_base_module_1_0_0_t;
typedef struct pmix_pstat_base_module_1_0_0_t pmix_pstat_base_module_t;

/**
 * Macro for use in components that are of type pstat
 */
#define PMIX_PSTAT_BASE_VERSION_1_0_0 PMIX_MCA_BASE_VERSION_1_0_0("pstat", 1, 0, 0)

/* Global structure for accessing pstat functions */
PMIX_EXPORT extern pmix_pstat_base_module_t pmix_pstat;

END_C_DECLS

#endif /* PMIX_MCA_PSTAT_H */
