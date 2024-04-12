/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
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
#include "constants.h"

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/mca.h"
#include "src/runtime/prte_globals.h"

#include "src/util/proc_info.h"

#include "grpcomm_direct.h"

static int my_priority = 5; /* must be below "bad" module */
static int direct_open(void);
static int direct_close(void);
static int direct_query(pmix_mca_base_module_t **module, int *priority);
static int direct_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
prte_grpcomm_base_component_t prte_mca_grpcomm_direct_component = {
    PRTE_GRPCOMM_BASE_VERSION_3_0_0,

    .pmix_mca_component_name = "direct",
    PMIX_MCA_BASE_MAKE_VERSION(component,
                               PRTE_MAJOR_VERSION,
                               PRTE_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),
    .pmix_mca_open_component = direct_open,
    .pmix_mca_close_component = direct_close,
    .pmix_mca_query_component = direct_query,
    .pmix_mca_register_component_params = direct_register,
};

static int direct_register(void)
{
    pmix_mca_base_component_t *c = &prte_mca_grpcomm_direct_component;

    /* make the priority adjustable so users can select
     * direct for use by apps without affecting daemons
     */
    my_priority = 85;
    (void) pmix_mca_base_component_var_register(c, "priority",
                                                "Priority of the grpcomm direct component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &my_priority);
    return PRTE_SUCCESS;
}

/* Open the component */
static int direct_open(void)
{
    return PRTE_SUCCESS;
}

static int direct_close(void)
{
    return PRTE_SUCCESS;
}

static int direct_query(pmix_mca_base_module_t **module, int *priority)
{
    /* we are always available */
    *priority = my_priority;
    *module = (pmix_mca_base_module_t *) &prte_grpcomm_direct_module;
    return PRTE_SUCCESS;
}
