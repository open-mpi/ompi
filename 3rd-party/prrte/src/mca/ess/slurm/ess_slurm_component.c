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
#include "src/mca/ess/slurm/ess_slurm.h"

extern prte_ess_base_module_t prte_ess_slurm_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
prte_ess_base_component_t prte_mca_ess_slurm_component = {
    PRTE_ESS_BASE_VERSION_3_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "slurm",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_open_component = prte_mca_ess_slurm_component_open,
    .pmix_mca_close_component = prte_mca_ess_slurm_component_close,
    .pmix_mca_query_component = prte_mca_ess_slurm_component_query,
};

int prte_mca_ess_slurm_component_open(void)
{
    return PRTE_SUCCESS;
}

int prte_mca_ess_slurm_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* Are we running under a SLURM job? Were
     * we given a path back to the HNP? If the
     * answer to both is "yes", then we were launched
     * by mpirun in a slurm world, so make ourselves available
     */

    if (PRTE_PROC_IS_DAEMON && NULL != getenv("SLURM_JOBID")
        && NULL != prte_process_info.my_hnp_uri) {
        *priority = 50;
        *module = (pmix_mca_base_module_t *) &prte_ess_slurm_module;
        return PRTE_SUCCESS;
    }

    /* Sadly, no */
    *priority = -1;
    *module = NULL;
    return PRTE_ERROR;
}

int prte_mca_ess_slurm_component_close(void)
{
    return PRTE_SUCCESS;
}
