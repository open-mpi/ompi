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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
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

#include <lsf/lsbatch.h>

#include "src/mca/base/pmix_base.h"

#include "ras_lsf.h"

/*
 * Local functions
 */

static int prte_ras_lsf_open(void);
static int prte_ras_lsf_close(void);
static int prte_mca_ras_lsf_component_query(pmix_mca_base_module_t **module, int *priority);
static int prte_ras_lsf_register(void);

bool prte_ras_lsf_skip_affinity_file = false;

prte_ras_base_component_t prte_mca_ras_lsf_component = {
    /* Indicate that we are a ras v2.0.0 component (which also
       implies a specific MCA version) */

    PRTE_RAS_BASE_VERSION_2_0_0,

    .pmix_mca_component_name = "lsf",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PRTE_RELEASE_VERSION),
    .pmix_mca_open_component = prte_ras_lsf_open,
    .pmix_mca_close_component = prte_ras_lsf_close,
    .pmix_mca_query_component = prte_mca_ras_lsf_component_query,
    .pmix_mca_register_component_params = prte_ras_lsf_register,
};

/**
 * component open/close/init function
 */
static int prte_ras_lsf_open(void)
{
    return PRTE_SUCCESS;
}

static int prte_mca_ras_lsf_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* check if lsf is running here */
    if (NULL == getenv("LSB_JOBID") || lsb_init("PRTE launcher") < 0) {
        /* nope, not here */
        *module = NULL;
        return PRTE_ERROR;
    }

    *priority = 75;
    *module = (pmix_mca_base_module_t *) &prte_ras_lsf_module;
    return PRTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int prte_ras_lsf_close(void)
{
    return PRTE_SUCCESS;
}

static int prte_ras_lsf_register(void)
{
    prte_ras_lsf_skip_affinity_file = false;
    (void) pmix_mca_base_component_var_register(&prte_mca_ras_lsf_component,
                                                "skip_affinity_file",
                                                "Skip processing the LSB_AFFINITY_HOSTFILE.",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_ras_lsf_skip_affinity_file);

    return PRTE_SUCCESS;
}
