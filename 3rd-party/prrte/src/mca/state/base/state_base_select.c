/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
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

#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"

#include "src/mca/state/base/base.h"

int prte_state_base_select(void)
{
    int exit_status = PRTE_SUCCESS;
    prte_state_base_component_t *best_component = NULL;
    prte_state_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    exit_status = pmix_mca_base_select("state",
                                       prte_state_base_framework.framework_output,
                                       &prte_state_base_framework.framework_components,
                                       (pmix_mca_base_module_t **) &best_module,
                                       (pmix_mca_base_component_t **) &best_component, NULL);
    if (PRTE_SUCCESS != exit_status || NULL == best_module) {
        /* This will only happen if no component was selected */
        exit_status = PRTE_ERROR;
        goto cleanup;
    }

    /* Save the winner */
    prte_state = *best_module;

    /* Initialize the winner */
    if (PRTE_SUCCESS != prte_state.init()) {
        exit_status = PRTE_ERROR;
        goto cleanup;
    }

cleanup:
    return exit_status;
}
