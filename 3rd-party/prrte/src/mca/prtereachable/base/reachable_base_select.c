/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/include/constants.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/prtereachable/base/base.h"
#include "src/mca/prtereachable/prtereachable.h"

/*
 * Globals
 */

int prte_reachable_base_select(void)
{
    int ret;
    prte_reachable_base_component_t *best_component = NULL;
    prte_reachable_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if (PRTE_SUCCESS
        != pmix_mca_base_select("prtereachable", prte_prtereachable_base_framework.framework_output,
                                &prte_prtereachable_base_framework.framework_components,
                                (pmix_mca_base_module_t **) &best_module,
                                (pmix_mca_base_component_t **) &best_component, NULL)) {
        /* notify caller that no available component found */
        return PRTE_ERR_NOT_FOUND;
    }

    /* Save the winner */
    prte_reachable = *best_module;

    /* Initialize the winner */
    ret = prte_reachable.init();

    return ret;
}
