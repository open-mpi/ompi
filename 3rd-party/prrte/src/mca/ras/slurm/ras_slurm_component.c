/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
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

#include "prte_config.h"
#include "constants.h"

#include "src/include/prte_socket_errno.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/util/pmix_net.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "ras_slurm.h"
#include "src/mca/ras/base/ras_private.h"

/*
 * Local functions
 */
static int ras_slurm_register(void);
static int ras_slurm_open(void);
static int ras_slurm_close(void);
static int prte_mca_ras_slurm_component_query(pmix_mca_base_module_t **module, int *priority);

prte_mca_ras_slurm_component_t prte_mca_ras_slurm_component = {
    .super = {
        PRTE_RAS_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "slurm",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = ras_slurm_open,
        .pmix_mca_close_component = ras_slurm_close,
        .pmix_mca_query_component = prte_mca_ras_slurm_component_query,
        .pmix_mca_register_component_params = ras_slurm_register
    }
};

static int ras_slurm_register(void)
{
    pmix_mca_base_component_t *component = &prte_mca_ras_slurm_component.super;

    prte_mca_ras_slurm_component.timeout = 30;
    (void) pmix_mca_base_component_var_register(component, "dyn_allocate_timeout",
                                                "Number of seconds to wait for Slurm dynamic allocation",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_ras_slurm_component.timeout);

    prte_mca_ras_slurm_component.dyn_alloc_enabled = false;
    (void) pmix_mca_base_component_var_register(component, "enable_dyn_alloc",
                                                "Whether or not dynamic allocations are enabled",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_ras_slurm_component.dyn_alloc_enabled);

    prte_mca_ras_slurm_component.config_file = NULL;
    (void) pmix_mca_base_component_var_register(component, "config_file",
                                                "Path to Slurm configuration file",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_ras_slurm_component.config_file);

    prte_mca_ras_slurm_component.rolling_alloc = false;
    (void) pmix_mca_base_component_var_register(component, "enable_rolling_alloc",
                                                "Enable partial dynamic allocations",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_ras_slurm_component.rolling_alloc);

    prte_mca_ras_slurm_component.use_all = false;
    (void) pmix_mca_base_component_var_register(component, "use_entire_allocation",
                                                "Use entire allocation (not just job step nodes) for this application",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_ras_slurm_component.use_all);

    return PRTE_SUCCESS;
}

static int ras_slurm_open(void)
{
    return PRTE_SUCCESS;
}

static int ras_slurm_close(void)
{
    return PRTE_SUCCESS;
}

static int prte_mca_ras_slurm_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* if I built, then slurm support is available. If
     * I am not in a Slurm allocation, and dynamic alloc
     * is not enabled, then disqualify myself
     */
    if (NULL == getenv("SLURM_JOBID") && !prte_mca_ras_slurm_component.dyn_alloc_enabled) {
        /* disqualify ourselves */
        *priority = 0;
        *module = NULL;
        return PRTE_ERROR;
    }

    PMIX_OUTPUT_VERBOSE((2, prte_ras_base_framework.framework_output,
                         "%s ras:slurm: available for selection",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
    /* since only one RM can exist on a cluster, just set
     * my priority to something - the other components won't
     * be responding anyway
     */
    *priority = 50;
    *module = (pmix_mca_base_module_t *) &prte_ras_slurm_module;
    return PRTE_SUCCESS;
}
