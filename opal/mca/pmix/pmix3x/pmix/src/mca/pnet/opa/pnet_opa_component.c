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
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include <src/include/pmix_config.h>
#include "pmix_common.h"

#include "src/util/argv.h"
#include "src/mca/pnet/pnet.h"
#include "pnet_opa.h"

static pmix_status_t component_open(void);
static pmix_status_t component_close(void);
static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_pnet_opa_component_t mca_pnet_opa_component = {
    .super = {
        .base = {
            PMIX_PNET_BASE_VERSION_1_0_0,

            /* Component name and version */
            .pmix_mca_component_name = "opa",
            PMIX_MCA_BASE_MAKE_VERSION(component,
                                       PMIX_MAJOR_VERSION,
                                       PMIX_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),

            /* Component open and close functions */
            .pmix_mca_open_component = component_open,
            .pmix_mca_close_component = component_close,
            .pmix_mca_register_component_params = component_register,
            .pmix_mca_query_component = component_query,
        },
        .data = {
            /* The component is checkpoint ready */
            PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },
    .include = NULL,
    .exclude = NULL
};

static pmix_status_t component_register(void)
{
    pmix_mca_base_component_t *component = &mca_pnet_opa_component.super.base;

    mca_pnet_opa_component.incparms = "HFI_*,PSM2_*";
    (void)pmix_mca_base_component_var_register(component, "include_envars",
                                               "Comma-delimited list of envars to harvest (\'*\' and \'?\' supported)",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                               PMIX_INFO_LVL_2,
                                               PMIX_MCA_BASE_VAR_SCOPE_LOCAL,
                                               &mca_pnet_opa_component.incparms);
    if (NULL != mca_pnet_opa_component.incparms) {
        mca_pnet_opa_component.include = pmix_argv_split(mca_pnet_opa_component.incparms, ',');
    }

    mca_pnet_opa_component.excparms = NULL;
    (void)pmix_mca_base_component_var_register(component, "exclude_envars",
                                               "Comma-delimited list of envars to exclude (\'*\' and \'?\' supported)",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                               PMIX_INFO_LVL_2,
                                               PMIX_MCA_BASE_VAR_SCOPE_LOCAL,
                                               &mca_pnet_opa_component.excparms);
    if (NULL != mca_pnet_opa_component.excparms) {
        mca_pnet_opa_component.exclude = pmix_argv_split(mca_pnet_opa_component.excparms, ',');
    }

    return PMIX_SUCCESS;
}

static pmix_status_t component_open(void)
{
    return PMIX_SUCCESS;
}


static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 10;
    *module = (pmix_mca_base_module_t *)&pmix_opa_module;
    return PMIX_SUCCESS;
}


static pmix_status_t component_close(void)
{
    return PMIX_SUCCESS;
}
