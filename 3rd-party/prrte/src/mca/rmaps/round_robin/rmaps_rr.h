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
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
 * Resource Mapping
 */
#ifndef PRTE_RMAPS_RR_H
#define PRTE_RMAPS_RR_H

#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/hwloc/hwloc-internal.h"

#include "src/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

PRTE_MODULE_EXPORT extern prte_rmaps_base_component_t prte_mca_rmaps_round_robin_component;
extern prte_rmaps_base_module_t prte_rmaps_round_robin_module;

PRTE_MODULE_EXPORT int prte_rmaps_rr_bynode(prte_job_t *jdata, prte_app_context_t *app,
                                            pmix_list_t *node_list, int32_t num_slots,
                                            pmix_rank_t nprocs, prte_rmaps_options_t *options);
PRTE_MODULE_EXPORT int prte_rmaps_rr_byslot(prte_job_t *jdata, prte_app_context_t *app,
                                            pmix_list_t *node_list, int32_t num_slots,
                                            pmix_rank_t nprocs, prte_rmaps_options_t *options);

PRTE_MODULE_EXPORT int prte_rmaps_rr_byobj(prte_job_t *jdata,
                                           prte_app_context_t *app,
                                           pmix_list_t *node_list,
                                           int32_t num_slots,
                                           pmix_rank_t num_procs,
                                           prte_rmaps_options_t *options);

PRTE_MODULE_EXPORT int prte_rmaps_rr_bycpu(prte_job_t *jdata, prte_app_context_t *app,
                                           pmix_list_t *node_list, int32_t num_slots,
                                           pmix_rank_t num_procs, prte_rmaps_options_t *options);

END_C_DECLS

#endif
