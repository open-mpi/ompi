/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
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

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_component_repository.h"
#include "src/mca/mca.h"

#include "src/util/proc_info.h"

#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"

/**
 * Function for selecting one component from all those that are
 * available.
 */

int prte_plm_base_select(void)
{
    int rc;
    prte_plm_base_component_t *best_component = NULL;
    prte_plm_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    rc = pmix_mca_base_select("plm", prte_plm_base_framework.framework_output,
                              &prte_plm_base_framework.framework_components,
                              (pmix_mca_base_module_t **) &best_module,
                              (pmix_mca_base_component_t **) &best_component, NULL);
    if (PMIX_SUCCESS == rc) {
        /* Save the winner */
        prte_plm = *best_module;
    } else {
        // leave the default in case they don't need a launcher - i.e.,
        // for the use-case of purely local operations. We will generate
        // an error when they try to launch daemons
        rc = PRTE_SUCCESS;
    }

    return rc;
}
