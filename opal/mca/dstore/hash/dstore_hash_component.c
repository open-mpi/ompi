/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/error.h"

#include "opal/mca/dstore/dstore.h"
#include "opal/mca/dstore/base/base.h"
#include "dstore_hash.h"

static opal_dstore_base_module_t *component_create(opal_list_t *attrs);
static int dstore_hash_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_dstore_base_component_t mca_dstore_hash_component = {
    .base_version = {
        OPAL_DSTORE_BASE_VERSION_2_0_0,

        /* Component name and version */
        .mca_component_name = "hash",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                              OPAL_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_query_component = dstore_hash_query,
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .create_handle = component_create,
};

static int dstore_hash_query(mca_base_module_t **module, int *priority)
{
    /* we are always available, but only as storage */
    *priority = 80;
    *module = NULL;
    return OPAL_SUCCESS;
}

/* this component ignores any input attributes */
static opal_dstore_base_module_t *component_create(opal_list_t *attrs)
{
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)malloc(sizeof(mca_dstore_hash_module_t));
    if (NULL == mod) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    /* copy the APIs across */
    memcpy(mod, &opal_dstore_hash_module.api, sizeof(opal_dstore_base_module_t));
    /* let the module init itself */
    if (OPAL_SUCCESS != mod->api.init((struct opal_dstore_base_module_t*)mod)) {
        /* release the module and return the error */
        free(mod);
        return NULL;
    }
    return (opal_dstore_base_module_t*)mod;
}
