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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "pmix_config.h"
#include "pmix_common.h"

#include "src/mca/mca.h"
#include "src/mca/base/base.h"

#include "src/mca/pfexec/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int pmix_pfexec_base_select(void)
{
    pmix_pfexec_base_component_t *best_component = NULL;
    pmix_pfexec_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if (PMIX_SUCCESS != pmix_mca_base_select("pfexec", pmix_pfexec_base_framework.framework_output,
                                             &pmix_pfexec_base_framework.framework_components,
                                             (pmix_mca_base_module_t **) &best_module,
                                             (pmix_mca_base_component_t **) &best_component, NULL) ) {
        /* This will only happen if no component was selected */
        return PMIX_ERR_NOT_FOUND;
    }

    /* Save the winner */
    pmix_pfexec = *best_module;

    return PMIX_SUCCESS;
}
