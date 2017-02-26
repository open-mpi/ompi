/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/mca/spml/base/base.h"

#include "sshmem_ucx.h"

/**
 * public string showing the shmem ompi_ucx component version number
 */
const char *mca_sshmem_ucx_component_version_string =
    "OSHMEM ucx sshmem MCA component version " OSHMEM_VERSION;


/**
 * local functions
 */
static int ucx_register(void);
static int ucx_open(void);
static int ucx_close(void);
static int ucx_query(mca_base_module_t **module, int *priority);
static int ucx_runtime_query(mca_base_module_t **module,
                             int *priority,
                             const char *hint);

/**
 * instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sshmem_ucx_component_t mca_sshmem_ucx_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /**
         * common MCA component data
         */
        .base_version = {
            MCA_SSHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            .mca_component_name = "ucx",
            MCA_BASE_MAKE_VERSION(component, OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION,
                                  OSHMEM_RELEASE_VERSION),

            .mca_open_component = ucx_open,
            .mca_close_component = ucx_close,
            .mca_query_component = ucx_query,
            .mca_register_component_params = ucx_register,
        },
        /* MCA v2.0.0 component meta data */
        .base_data = {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .runtime_query = ucx_runtime_query,
    },
};

static int
ucx_runtime_query(mca_base_module_t **module,
                   int *priority,
                   const char *hint)
{
    /* check that spml ucx was selected. Otherwise disqualify */
    if (strcmp(mca_spml_base_selected_component.spmlm_version.mca_component_name, "ucx")) {
        *module = NULL;
        return OSHMEM_ERR_NOT_AVAILABLE;
    }

    *priority = mca_sshmem_ucx_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_ucx_module.super;
    return OPAL_SUCCESS;
}

static int
ucx_register(void)
{
    /* (default) priority - set high to make ucx the default */
    mca_sshmem_ucx_component.priority = 100;
    mca_base_component_var_register (&mca_sshmem_ucx_component.super.base_version,
                                     "priority", "Priority for sshmem ucx "
                                     "component (default: 100)", MCA_BASE_VAR_TYPE_INT,
                                     NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                     OPAL_INFO_LVL_3,
                                     MCA_BASE_VAR_SCOPE_ALL_EQ,
                                     &mca_sshmem_ucx_component.priority);

    return OSHMEM_SUCCESS;
}

static int
ucx_open(void)
{
    return OSHMEM_SUCCESS;
}

static int
ucx_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_sshmem_ucx_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_ucx_module.super;
    return OSHMEM_SUCCESS;
}

static int
ucx_close(void)
{
    return OSHMEM_SUCCESS;
}

