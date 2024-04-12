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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef PRTE_MCA_RAS_BASE_H
#define PRTE_MCA_RAS_BASE_H

/*
 * includes
 */
#include "prte_config.h"
#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/ras/ras.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_printf.h"
/*
 * Global functions for MCA overall collective open and close
 */

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_ras_base_framework;
/* select a component */
PRTE_EXPORT int prte_ras_base_select(void);

/*
 * globals that might be needed
 */
typedef struct prte_ras_base_t {
    bool allocation_read;
    prte_ras_base_module_t *active_module;
    int total_slots_alloc;
    int multiplier;
    bool launch_orted_on_hn;
    bool simulated;
} prte_ras_base_t;

PRTE_EXPORT extern prte_ras_base_t prte_ras_base;

PRTE_EXPORT void prte_ras_base_display_alloc(prte_job_t *jdata);

PRTE_EXPORT void prte_ras_base_display_cpus(prte_job_t *jdata, char *nodelist);

PRTE_EXPORT void prte_ras_base_allocate(int fd, short args, void *cbdata);

PRTE_EXPORT int prte_ras_base_add_hosts(prte_job_t *jdata);

PRTE_EXPORT char *prte_ras_base_flag_string(prte_node_t *node);

END_C_DECLS

#endif
