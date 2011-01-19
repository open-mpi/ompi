/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Test C99 struct initialization. Remove on 1/20/2011 after MTT has run.
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "dummy.h"

static int component_open(void);
static int component_query(mca_base_module_t **module, int *priority);

/*
 * Struct of function pointers that need to be initialized
 */
orte_debugger_base_component_t mca_debugger_dummy_component = {
    .base_version = {
        ORTE_DEBUGGER_BASE_VERSION_1_0_0,
        
        .mca_component_name            = "dummy",
        .mca_component_major_version   = ORTE_MAJOR_VERSION,
        .mca_component_minor_version   = ORTE_MINOR_VERSION,
        .mca_component_release_version = ORTE_RELEASE_VERSION,  /* MCA module release version */

        .mca_open_component  = component_open,
        .mca_close_component = NULL,
        .mca_query_component = component_query
    },
    .base_data = {
        /* The component is checkpoint ready */
        .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int component_open(void)
{
    mca_base_component_t *c = &mca_debugger_dummy_component.base_version;

    return ORTE_SUCCESS;
}

static int component_query(mca_base_module_t **module, int *priority)
{
    *priority = 10;
    *module = (mca_base_module_t *)&orte_debugger_dummy_module;
    return ORTE_SUCCESS;
}
