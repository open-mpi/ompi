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
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "prte_config.h"
#include "constants.h"

#include "src/util/proc_info.h"

#include "src/mca/ess/ess.h"
#include "src/mca/ess/env/ess_env.h"

extern prte_ess_base_module_t prte_ess_env_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
prte_ess_base_component_t prte_mca_ess_env_component = {
    PRTE_ESS_BASE_VERSION_3_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "env",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_open_component = prte_mca_ess_env_component_open,
    .pmix_mca_close_component = prte_mca_ess_env_component_close,
    .pmix_mca_query_component = prte_mca_ess_env_component_query,
};

int prte_mca_ess_env_component_open(void)
{
    return PRTE_SUCCESS;
}

int prte_mca_ess_env_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* we are the env module, only used by daemons that are
     * launched by ssh so allow any enviro-specifc modules
     * to override us */
    if (PRTE_PROC_IS_DAEMON) {
        *priority = 1;
        *module = (pmix_mca_base_module_t *) &prte_ess_env_module;
        return PRTE_SUCCESS;
    }

    /* if not, then return NULL - we cannot be selected */
    *priority = -1;
    *module = NULL;
    return PRTE_ERROR;
}

int prte_mca_ess_env_component_close(void)
{
    return PRTE_SUCCESS;
}
