/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2017-2019 IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "opal/util/show_help.h"

#include "orte/mca/schizo/schizo.h"
#include "schizo_jsm.h"

static int component_query(mca_base_module_t **module, int *priority);

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_schizo_base_component_t mca_schizo_jsm_component = {
    .base_version = {
        MCA_SCHIZO_BASE_VERSION_1_0_0,
        .mca_component_name = "jsm",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_query_component = component_query,
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static int component_query(mca_base_module_t **module, int *priority)
{
    /* disqualify ourselves if we are not under jsm */
    if (NULL == getenv("JSM_JSRUN_PORT")) {
        *priority = 0;
        *module = NULL;
        return OPAL_ERROR;
    }

    *module = (mca_base_module_t*)&orte_schizo_jsm_module;
    *priority = 80;
    return ORTE_SUCCESS;
}
