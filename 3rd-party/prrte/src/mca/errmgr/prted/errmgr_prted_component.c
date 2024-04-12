/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
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
#include "src/util/pmix_output.h"

#include "errmgr_prted.h"
#include "src/mca/errmgr/base/base.h"
#include "src/mca/errmgr/errmgr.h"

/*
 * Public string for version number
 */
const char *prte_mca_errmgr_prted_component_version_string
    = "PRTE ERRMGR prted MCA component version " PRTE_VERSION;

/*
 * Local functionality
 */
static int errmgr_prted_register(void);
static int errmgr_prted_open(void);
static int errmgr_prted_close(void);
static int errmgr_prted_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
prte_errmgr_base_component_t prte_mca_errmgr_prted_component =
{
    /* Handle the general mca_component_t struct containing
     *  meta information about the component itprted
     */
    .base_version = {
        PRTE_ERRMGR_BASE_VERSION_3_0_0,
        /* Component name and version */
        .pmix_mca_component_name = "prted",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = errmgr_prted_open,
        .pmix_mca_close_component = errmgr_prted_close,
        .pmix_mca_query_component = errmgr_prted_component_query,
        .pmix_mca_register_component_params = errmgr_prted_register,
    }
};

static int my_priority;

static int errmgr_prted_register(void)
{
    pmix_mca_base_component_t *c = &prte_mca_errmgr_prted_component.base_version;

    my_priority = 1000;
    (void) pmix_mca_base_component_var_register(c, "priority",
                                                "Priority of the prted errmgr component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &my_priority);

    return PRTE_SUCCESS;
}

static int errmgr_prted_open(void)
{
    return PRTE_SUCCESS;
}

static int errmgr_prted_close(void)
{
    return PRTE_SUCCESS;
}

static int errmgr_prted_component_query(pmix_mca_base_module_t **module, int *priority)
{
    if (PRTE_PROC_IS_DAEMON) {
        /* we are the default component for daemons */
        *priority = my_priority;
        *module = (pmix_mca_base_module_t *) &prte_errmgr_prted_module;
        return PRTE_SUCCESS;
    }

    *priority = -1;
    *module = NULL;
    return PRTE_ERROR;
}
