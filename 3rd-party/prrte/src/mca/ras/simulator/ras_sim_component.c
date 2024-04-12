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
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
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

#include "src/mca/base/pmix_base.h"

#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "ras_sim.h"
#include "src/mca/ras/base/ras_private.h"

/*
 * Local functions
 */
static int ras_sim_register(void);
static int ras_sim_component_query(pmix_mca_base_module_t **module, int *priority);

prte_ras_sim_component_t prte_mca_ras_simulator_component = {
    .super = {
        PRTE_RAS_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "simulator",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),
        .pmix_mca_query_component = ras_sim_component_query,
        .pmix_mca_register_component_params = ras_sim_register
    }
};

static int ras_sim_register(void)
{
    pmix_mca_base_component_t *component = &prte_mca_ras_simulator_component.super;

    prte_mca_ras_simulator_component.slots = NULL;
    (void) pmix_mca_base_component_var_register(component, "slots",
                                                "Comma-separated list of number of slots on each node to simulate",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_ras_simulator_component.slots);

    prte_mca_ras_simulator_component.slots_max = NULL;
    (void) pmix_mca_base_component_var_register(component, "max_slots",
                                                "Comma-separated list of number of max slots on each node to simulate",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_ras_simulator_component.slots_max);

    prte_mca_ras_simulator_component.num_nodes = NULL;
    (void) pmix_mca_base_component_var_register(component, "num_nodes",
                                                "Comma-separated list of number of nodes to simulate for each topology",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_ras_simulator_component.num_nodes);

    prte_mca_ras_simulator_component.have_cpubind = true;
    (void) pmix_mca_base_component_var_register(component, "have_cpubind",
                                                "Topology supports binding to cpus",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_ras_simulator_component.have_cpubind);

    prte_mca_ras_simulator_component.have_membind = true;
    (void) pmix_mca_base_component_var_register(component, "have_membind",
                                                "Topology supports binding to memory",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_ras_simulator_component.have_membind);
    return PRTE_SUCCESS;
}

static int ras_sim_component_query(pmix_mca_base_module_t **module, int *priority)
{
    if (NULL != prte_mca_ras_simulator_component.num_nodes) {
        *module = (pmix_mca_base_module_t *) &prte_ras_sim_module;
        *priority = 1000;
        prte_ras_base.simulated = true;
        return PRTE_SUCCESS;
    }

    /* Sadly, no */
    *module = NULL;
    *priority = 0;
    return PRTE_ERROR;
}
