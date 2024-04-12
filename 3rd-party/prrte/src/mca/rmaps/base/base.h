/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * rmaps framework base functionality.
 */

#ifndef PRTE_MCA_RMAPS_BASE_H
#define PRTE_MCA_RMAPS_BASE_H

/*
 * includes
 */
#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/schizo/schizo.h"
#include "src/util/pmix_printf.h"

#include "src/runtime/prte_globals.h"

#include "src/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_rmaps_base_framework;
/* select a component */
PRTE_EXPORT int prte_rmaps_base_select(void);

/*
 * Global functions for MCA overall collective open and close
 */

/**
 * Struct to hold data global to the rmaps framework
 */
typedef struct {
    /* list of selected modules */
    pmix_list_t selected_modules;
    /* default mapping/ranking directives */
    prte_mapping_policy_t mapping;
    prte_ranking_policy_t ranking;
    /* default device for dist mapping */
    char *device;
    /* whether or not child jobs should inherit mapping/ranking/binding directives from their parent
     * by default */
    bool inherit;
    /* whether or not we are using hwthreads as independent cpus by default */
    bool hwthread_cpus;
    /* default file for use in sequential and rankfile mapping
     * when the directive comes thru MCA param */
    char *file;
    hwloc_cpuset_t available, baseset;  // scratch for binding calculation
    char *default_mapping_policy;
    /* whether or not to require hwtcpus due to topology limitations */
    bool require_hwtcpus;
} prte_rmaps_base_t;

/**
 * Global instance of rmaps-wide framework data
 */
PRTE_EXPORT extern prte_rmaps_base_t prte_rmaps_base;

/**
 * Select an rmaps component / module
 */
typedef struct {
    pmix_list_item_t super;
    int pri;
    prte_rmaps_base_module_t *module;
    pmix_mca_base_component_t *component;
} prte_rmaps_base_selected_module_t;
PMIX_CLASS_DECLARATION(prte_rmaps_base_selected_module_t);

/*
 * Map a job
 */
PRTE_EXPORT void prte_rmaps_base_map_job(int sd, short args, void *cbdata);

/**
 * Utility routines to get/set vpid mapping for the job
 */

PRTE_EXPORT int prte_rmaps_base_get_vpid_range(pmix_nspace_t jobid, pmix_rank_t *start,
                                               pmix_rank_t *range);
PRTE_EXPORT int prte_rmaps_base_set_vpid_range(pmix_nspace_t jobid, pmix_rank_t start,
                                               pmix_rank_t range);

/* pretty-print functions */
PRTE_EXPORT char *prte_rmaps_base_print_mapping(prte_mapping_policy_t mapping);
PRTE_EXPORT char *prte_rmaps_base_print_ranking(prte_ranking_policy_t ranking);

PRTE_EXPORT int prte_rmaps_base_prep_topology(hwloc_topology_t topo);

PRTE_EXPORT int prte_rmaps_base_filter_nodes(prte_app_context_t *app, pmix_list_t *nodes,
                                             bool remove);

PRTE_EXPORT int prte_rmaps_base_set_default_mapping(prte_job_t *jdata,
                                                    prte_rmaps_options_t *options);
PRTE_EXPORT int prte_rmaps_base_set_mapping_policy(prte_job_t *jdata, char *spec);

PRTE_EXPORT int prte_rmaps_base_set_default_ranking(prte_job_t *jdata,
                                                    prte_rmaps_options_t *options);
PRTE_EXPORT int prte_rmaps_base_set_ranking_policy(prte_job_t *jdata, char *spec);

PRTE_EXPORT void prte_rmaps_base_display_map(prte_job_t *jdata);
PRTE_EXPORT void prte_rmaps_base_report_bindings(prte_job_t *jdata,
                                                 prte_rmaps_options_t *options);

PRTE_EXPORT int prte_rmaps_base_get_ncpus(prte_node_t *node,
                                          hwloc_obj_t obj,
                                          prte_rmaps_options_t *options);

PRTE_EXPORT bool prte_rmaps_base_check_avail(prte_job_t *jdata,
                                             prte_app_context_t *app,
                                             prte_node_t *node,
                                             pmix_list_t *node_list,
                                             hwloc_obj_t obj,
                                             prte_rmaps_options_t *options);

PRTE_EXPORT int prte_rmaps_base_check_oversubscribed(prte_job_t *jdata,
                                                     prte_app_context_t *app,
                                                     prte_node_t *node,
                                                     prte_rmaps_options_t *options);

PRTE_EXPORT void prte_rmaps_base_get_cpuset(prte_job_t *jdata,
                                            prte_node_t *node,
                                            prte_rmaps_options_t *options);

PRTE_EXPORT int prte_rmaps_base_check_support(prte_job_t *jdata,
                                              prte_node_t *node,
                                              prte_rmaps_options_t *options);

END_C_DECLS

#endif
