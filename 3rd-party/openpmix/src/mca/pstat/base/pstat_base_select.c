/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "pmix_common.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/pstat/base/base.h"
#include "src/mca/pstat/pstat.h"

/*
 * Globals
 */

int pmix_pstat_base_select(void)
{
    int ret, exit_status = PMIX_SUCCESS;
    pmix_pstat_base_component_t *best_component = NULL;
    pmix_pstat_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if (PMIX_SUCCESS
        != pmix_mca_base_select("pstat", pmix_pstat_base_framework.framework_output,
                                &pmix_pstat_base_framework.framework_components,
                                (pmix_mca_base_module_t **) &best_module,
                                (pmix_mca_base_component_t **) &best_component, NULL)) {
        /* It is okay if we don't find a runnable component - default
         * to the unsupported default.
         */
        goto cleanup;
    }

    /* Save the winner */
    pmix_pstat_base_component = best_component;
    pmix_pstat = *best_module;

    /* Initialize the winner */
    if (PMIX_SUCCESS != (ret = pmix_pstat.init())) {
        exit_status = ret;
        goto cleanup;
    }

cleanup:
    return exit_status;
}
