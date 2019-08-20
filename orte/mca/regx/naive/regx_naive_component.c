/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
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

#include "orte/mca/regx/regx.h"
#include "regx_naive.h"

static int component_query(mca_base_module_t **module, int *priority);
static int component_register(void);

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_regx_naive_component_t mca_regx_naive_component = {
    {
        .base_version = {
            MCA_REGX_BASE_VERSION_1_0_0,
            .mca_component_name = "naive",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                    ORTE_RELEASE_VERSION),
            .mca_query_component = component_query,
            .mca_register_component_params = component_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    }
};

static int component_query(mca_base_module_t **module, int *priority)
{
    *module = (mca_base_module_t*)&orte_regx_naive_module;
    *priority = 1;
    return ORTE_SUCCESS;
}

static int component_register(void)
{
    mca_base_component_t *c = &mca_regx_naive_component.super.base_version;

    mca_regx_naive_component.compress_vpids = false;
    (void) mca_base_component_var_register (c, "compress_vpids", "Enable compression of vpids (default: false)",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_regx_naive_component.compress_vpids);

    return ORTE_SUCCESS;
}
