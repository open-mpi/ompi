/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
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

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include "prm_pbs.h"
#include "src/mca/prm/prm.h"

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_prm_base_component_t pmix_mca_prm_pbs_component = {
    PMIX_PRM_BASE_VERSION_1_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "pbs",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PMIX_MAJOR_VERSION,
                               PMIX_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_query_component = component_query,
};

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority)
{
    if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)) {
        *priority = 0;
        *module = NULL;
        return PMIX_ERROR;
    }

    if (NULL != getenv("PBS_ENVIRONMENT") && NULL != getenv("PBS_JOBID")) {
        *priority = 25;
        *module = (pmix_mca_base_module_t *) &pmix_prm_pbs_module;
        return PMIX_SUCCESS;
    } else if (NULL != getenv("COBALT_JOBID")) {
        *priority = 25;
        *module = (pmix_mca_base_module_t *) &pmix_prm_pbs_module;
        return PMIX_SUCCESS;
    } else {
        *priority = 0;
        *module = NULL;
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
}
