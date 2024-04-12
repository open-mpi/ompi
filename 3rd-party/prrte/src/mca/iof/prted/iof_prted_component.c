/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/mca/base/pmix_base.h"

#include "src/util/proc_info.h"

#include "iof_prted.h"

/*
 * Local functions
 */
static int prte_iof_prted_open(void);
static int prte_iof_prted_close(void);
static int prte_iof_prted_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Public string showing the iof prted component version number
 */
const char *prte_mca_iof_prted_component_version_string
    = "PRTE prted iof MCA component version " PRTE_VERSION;

prte_mca_iof_prted_component_t prte_mca_iof_prted_component = {
    .super = {
        PRTE_IOF_BASE_VERSION_2_0_0,

        .pmix_mca_component_name = "prted",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open, close, and query functions */
        .pmix_mca_open_component = prte_iof_prted_open,
        .pmix_mca_close_component = prte_iof_prted_close,
        .pmix_mca_query_component = prte_iof_prted_query,
    }
};

/**
 * component open/close/init function
 */
static int prte_iof_prted_open(void)
{
    /* Nothing to do */
    return PRTE_SUCCESS;
}

static int prte_iof_prted_close(void)
{
    return PRTE_SUCCESS;
}

static int prte_iof_prted_query(pmix_mca_base_module_t **module, int *priority)
{
    /* if we are not a daemon, then don't use this module */
    if (!PRTE_PROC_IS_DAEMON) {
        *module = NULL;
        *priority = -1;
        return PRTE_ERROR;
    }

    *priority = 80;
    *module = (pmix_mca_base_module_t *) &prte_iof_prted_module;

    return PRTE_SUCCESS;
}
