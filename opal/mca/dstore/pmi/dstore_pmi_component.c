/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/pmix/pmix.h"

#include "opal/mca/base/base.h"

#include "opal/mca/dstore/dstore.h"
#include "opal/mca/dstore/base/base.h"
#include "opal/runtime/opal_params.h"
#include "dstore_pmi.h"

static void component_finalize(void);
static int setup_pmi(void);
static int dstore_pmi_query(mca_base_module_t **module, int *priority);
/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_dstore_base_component_t mca_dstore_pmi_component = {
    {
        OPAL_DSTORE_BASE_VERSION_2_0_0,

        /* Component name and version */
        "pmi",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        dstore_pmi_query,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    NULL,
    component_finalize
};

static void component_finalize(void)
{
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
    }
}

static int dstore_pmi_query(mca_base_module_t **module, int *priority)
{
    /* only use PMI if available */
    if (NULL != opal_pmix.init && OPAL_SUCCESS == opal_pmix.init()) {
        *priority = 1;
        *module = (mca_base_module_t *)&opal_dstore_pmi_module;
        return ORTE_SUCCESS;
    }
    /* if not, then we are not available */
    *priority = -1;
    *module = NULL;
    return OPAL_ERROR;
}
