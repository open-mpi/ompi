/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "src/util/pmix_output.h"

#include "src/mca/state/base/base.h"
#include "src/mca/state/state.h"
#include "state_dvm.h"

/*
 * Public string for version number
 */
const char *prte_mca_state_dvm_component_version_string
    = "PRTE STATE dvm MCA component version " PRTE_VERSION;

/*
 * Local functionality
 */
static int state_dvm_open(void);
static int state_dvm_close(void);
static int state_dvm_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
prte_state_base_component_t prte_mca_state_dvm_component =
{
    PRTE_STATE_BASE_VERSION_1_0_0,
    /* Component name and version */
    .pmix_mca_component_name = "dvm",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_open_component = state_dvm_open,
    .pmix_mca_close_component = state_dvm_close,
    .pmix_mca_query_component = state_dvm_component_query,
};

static int state_dvm_open(void)
{
    return PRTE_SUCCESS;
}

static int state_dvm_close(void)
{
    return PRTE_SUCCESS;
}

static int state_dvm_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* used by DVM masters */
    if (PRTE_PROC_IS_MASTER) {
        *priority = 100;
        *module = (pmix_mca_base_module_t *) &prte_state_dvm_module;
        return PRTE_SUCCESS;
    }

    *priority = 0;
    *module = NULL;
    return PRTE_ERR_NOT_AVAILABLE;
}
