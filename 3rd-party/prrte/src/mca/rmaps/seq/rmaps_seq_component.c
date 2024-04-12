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
#include "constants.h"

#include "src/mca/base/pmix_base.h"

#include "rmaps_seq.h"
#include "src/mca/rmaps/rmaps.h"

/*
 * Local functions
 */

static int prte_rmaps_seq_register(void);
static int prte_rmaps_seq_open(void);
static int prte_rmaps_seq_close(void);
static int prte_rmaps_seq_query(pmix_mca_base_module_t **module, int *priority);

static int my_priority;

prte_rmaps_base_component_t prte_mca_rmaps_seq_component = {
    PRTE_RMAPS_BASE_VERSION_4_0_0,

    .pmix_mca_component_name = "seq",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),
    .pmix_mca_open_component = prte_rmaps_seq_open,
    .pmix_mca_close_component = prte_rmaps_seq_close,
    .pmix_mca_query_component = prte_rmaps_seq_query,
    .pmix_mca_register_component_params = prte_rmaps_seq_register,
};

/**
 * component register/open/close/init function
 */
static int prte_rmaps_seq_register(void)
{
    my_priority = 60;
    (void) pmix_mca_base_component_var_register(&prte_mca_rmaps_seq_component, "priority",
                                                "Priority of the seq rmaps component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &my_priority);
    return PRTE_SUCCESS;
}

static int prte_rmaps_seq_open(void)
{
    return PRTE_SUCCESS;
}

static int prte_rmaps_seq_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = my_priority;
    *module = (pmix_mca_base_module_t *) &prte_rmaps_seq_module;
    return PRTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int prte_rmaps_seq_close(void)
{
    return PRTE_SUCCESS;
}
