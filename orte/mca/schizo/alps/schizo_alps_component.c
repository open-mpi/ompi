/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
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
#include "schizo_alps.h"

static int component_query(mca_base_module_t **module, int *priority);

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_schizo_base_component_t mca_schizo_alps_component = {
    .base_version = {
        MCA_SCHIZO_BASE_VERSION_1_0_0,
        .mca_component_name = "alps",
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
    /* if we are not an app, then don't bother */
    if (!ORTE_PROC_IS_APP) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    /* since we were built, assume we are on an alps system */
    *priority = 90;
    *module = (mca_base_module_t *)&orte_schizo_alps_module;
    return ORTE_SUCCESS;
}

