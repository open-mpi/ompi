/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
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
#include "src/mca/prtedl/prtedl.h"
#include "src/util/pmix_argv.h"

#include "prtedl_dlopen.h"

/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *prte_mca_prtedl_dlopen_component_version_string
    = "PRTE prtedl dlopen MCA component version " PRTE_VERSION;

/*
 * Local functions
 */
static int dlopen_component_register(void);
static int dlopen_component_open(void);
static int dlopen_component_close(void);
static int dlopen_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

prte_prtedl_dlopen_component_t prte_mca_prtedl_dlopen_component = {
    .base = {
        /* Fill in the mca_prtedl_base_component_t */
        .base_version = {
            PRTE_DL_BASE_VERSION_1_0_0,

            /* Component name and version */
            .pmix_mca_component_name = "dlopen",
            PMIX_MCA_BASE_MAKE_VERSION(component,
                                       PRTE_MAJOR_VERSION,
                                       PRTE_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),

            /* Component functions */
            .pmix_mca_register_component_params = dlopen_component_register,
            .pmix_mca_open_component = dlopen_component_open,
            .pmix_mca_close_component = dlopen_component_close,
            .pmix_mca_query_component = dlopen_component_query,
        },
            /* The prtedl framework members */
            .priority = 80
        },
};

static int dlopen_component_register(void)
{
    int ret;

    prte_mca_prtedl_dlopen_component.filename_suffixes_mca_storage = ".so,.dylib,.dll,.sl";
    ret = pmix_mca_base_component_var_register(&prte_mca_prtedl_dlopen_component.base.base_version,
                                               "filename_suffixes",
                                               "Comma-delimited list of filename suffixes that the PRTE dlopen component will try",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING,
                                               &prte_mca_prtedl_dlopen_component.filename_suffixes_mca_storage);
    if (ret < 0) {
        return ret;
    }
    prte_mca_prtedl_dlopen_component.filename_suffixes
        = PMIX_ARGV_SPLIT_COMPAT(prte_mca_prtedl_dlopen_component.filename_suffixes_mca_storage, ',');

    return PRTE_SUCCESS;
}

static int dlopen_component_open(void)
{
    return PRTE_SUCCESS;
}

static int dlopen_component_close(void)
{
    if (NULL != prte_mca_prtedl_dlopen_component.filename_suffixes) {
        PMIX_ARGV_FREE_COMPAT(prte_mca_prtedl_dlopen_component.filename_suffixes);
        prte_mca_prtedl_dlopen_component.filename_suffixes = NULL;
    }

    return PRTE_SUCCESS;
}

static int dlopen_component_query(pmix_mca_base_module_t **module, int *priority)
{
    /* The priority value is somewhat meaningless here; by
       src/mca/prtedl/configure.m4, there's at most one component
       available. */
    *priority = prte_mca_prtedl_dlopen_component.base.priority;
    *module = &prte_prtedl_dlopen_module.super;

    return PRTE_SUCCESS;
}
