/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.
 *                         All rights reserved
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

#include <string.h>

#include "constants.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"

#include "src/mca/filem/base/base.h"
#include "src/mca/filem/filem.h"

int prte_filem_base_select(void)
{
    int exit_status = PRTE_SUCCESS;
    prte_filem_base_component_t *best_component = NULL;
    prte_filem_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if (PRTE_SUCCESS
        != pmix_mca_base_select("filem", prte_filem_base_framework.framework_output,
                                &prte_filem_base_framework.framework_components,
                                (pmix_mca_base_module_t **) &best_module,
                                (pmix_mca_base_component_t **) &best_component, NULL)) {
        /* It is okay to not select anything - we'll just retain
         * the default none module
         */
        return PRTE_SUCCESS;
    }

    /* Save the winner */
    prte_filem = *best_module;

    /* Initialize the winner */
    if (NULL != prte_filem.filem_init) {
        if (PRTE_SUCCESS != prte_filem.filem_init()) {
            exit_status = PRTE_ERROR;
        }
    }

    return exit_status;
}
