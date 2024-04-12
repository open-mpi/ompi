/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#ifdef HAVE_UNISTD_H
#    include "unistd.h"
#endif

#include "src/include/constants.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/prtedl/base/base.h"
#include "src/mca/prtedl/prtedl.h"
#include "src/util/pmix_output.h"

int prte_dl_base_select(void)
{
    int exit_status = PRTE_SUCCESS;
    prte_prtedl_base_component_t *best_component = NULL;
    prte_prtedl_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if (PRTE_SUCCESS
        != pmix_mca_base_select("prtedl", prte_prtedl_base_framework.framework_output,
                                &prte_prtedl_base_framework.framework_components,
                                (pmix_mca_base_module_t **) &best_module,
                                (pmix_mca_base_component_t **) &best_component, NULL)) {
        /* This will only happen if no component was selected */
        exit_status = PRTE_ERROR;
        goto cleanup;
    }

    /* Save the winner */
    prte_prtedl_base_selected_component = best_component;
    prte_prtedl = best_module;

cleanup:
    return exit_status;
}
