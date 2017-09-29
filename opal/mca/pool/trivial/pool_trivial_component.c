/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/pool/pool.h"
#include "opal/mca/pool/base/base.h"


#include "pool_trivial.h"

static int initialized = 0;

static int trivial_open()
{
    return OPAL_SUCCESS;
}

static int trivial_close()
{
    return OPAL_SUCCESS;
}

static int trivial_register()
{
    return OPAL_SUCCESS;
}

static int trivial_query (int *priority)
{
    initialized = 1;
    *priority = 1;
    return OPAL_SUCCESS;
}

static char * trivial_get (void *handle)
{
    assert(initialized);
    return (char *)handle;
}

static void * trivial_put (char * str)
{
    assert(initialized);
    return str;
}

static void trivial_free (void *handle)
{
    assert(initialized);
    free(handle);
}

opal_pool_trivial_component_t mca_pool_trivial_component = {
    .super = {
        .base_version = {
            OPAL_POOL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "trivial",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = trivial_open,
            .mca_close_component = trivial_close,
            .mca_register_component_params = trivial_register,
        },
        .data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Memory framework functions. */
        .query = trivial_query,
        .get = trivial_get,
        .put = trivial_put,
        .free = trivial_free,
    },
};
