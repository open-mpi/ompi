/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>
#include "pmix_common.h"

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/psec/psec.h"
#include "psec_dummy_handshake.h"

static pmix_status_t component_open(void);
static pmix_status_t component_close(void);
static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_psec_module_t* assign_module(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_psec_base_component_t mca_psec_dummy_handshake_component = {
    .base = {
        PMIX_PSEC_BASE_VERSION_1_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "dummy_handshake",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PMIX_MAJOR_VERSION,
                                   PMIX_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = component_open,
        .pmix_mca_close_component = component_close,
        .pmix_mca_query_component = component_query,
    },
    .data = {
        /* The component is checkpoint ready */
        PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .assign_module = assign_module
};

static int component_open(void)
{
    return PMIX_SUCCESS;
}

static int component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 100;
    *module = (pmix_mca_base_module_t *)&pmix_dummy_handshake_module;
    return PMIX_SUCCESS;
}


static int component_close(void)
{
    return PMIX_SUCCESS;
}

static pmix_psec_module_t* assign_module(void)
{
    return &pmix_dummy_handshake_module;
}
