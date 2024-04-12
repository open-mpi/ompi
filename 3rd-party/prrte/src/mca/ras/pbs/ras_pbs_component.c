/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/util/pmix_basename.h"

#include "ras_pbs.h"
#include "src/mca/ras/base/ras_private.h"

/*
 * Local variables
 */
static int param_priority;

/*
 * Local functions
 */
static int ras_pbs_register(void);
static int ras_pbs_open(void);
static int prte_mca_ras_pbs_component_query(pmix_mca_base_module_t **module, int *priority);

prte_mca_ras_pbs_component_t prte_mca_ras_pbs_component = {
    .super = {
        PRTE_RAS_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "pbs",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = ras_pbs_open,
        .pmix_mca_query_component = prte_mca_ras_pbs_component_query,
        .pmix_mca_register_component_params = ras_pbs_register,
    }
};

static int ras_pbs_register(void)
{
    pmix_mca_base_component_t *c = &prte_mca_ras_pbs_component.super;

    param_priority = 100;
    (void) pmix_mca_base_component_var_register(c, "priority", "Priority of the pbs ras component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT, &param_priority);

    /* for big SMP machines (e.g., those from SGI), listing the nodes
     * once/slot in the nodefile is extreme. In those cases, they may
     * choose to list each node once, but then provide an envar that
     * tells us how many cpus/node were allocated. Allow the user to
     * inform us that we are in such an environment
     */
    prte_mca_ras_pbs_component.smp_mode = false;
    (void) pmix_mca_base_component_var_register(c, "smp",
                                                "The PBS system is configured in SMP mode "
                                                "with the number of cpus/node given in the environment",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL, &prte_mca_ras_pbs_component.smp_mode);

    return PRTE_SUCCESS;
}

static int ras_pbs_open(void)
{
    return PRTE_SUCCESS;
}

static int prte_mca_ras_pbs_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* Are we running under a PBS job? */
    if (NULL != getenv("PBS_ENVIRONMENT") && NULL != getenv("PBS_JOBID")) {
        *priority = param_priority;
        *module = (pmix_mca_base_module_t *) &prte_ras_pbs_module;
        return PRTE_SUCCESS;
    } else if (NULL != getenv("COBALT_JOBID")) {
        /* we are running under Argonne's Cobalt variant */
        *priority = param_priority;
        *module = (pmix_mca_base_module_t *) &prte_ras_pbs_module;
        return PRTE_SUCCESS;
    }
    /* Sadly, no */
    *module = NULL;
    return PRTE_ERROR;
}
