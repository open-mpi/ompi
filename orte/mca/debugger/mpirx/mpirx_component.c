/* -*- C -*-
 *
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "mpirx.h"

int orte_debugger_mpirx_check_rate=0;

static int component_open(void);
static int component_query(mca_base_module_t **module, int *priority);

/*
 * Struct of function pointers that need to be initialized
 */
orte_debugger_base_component_t mca_debugger_mpirx_component = {
    {
        ORTE_DEBUGGER_BASE_VERSION_1_0_0,
        
        "mpirx", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */

        component_open,
        NULL,
        component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int component_open(void)
{
    mca_base_component_t *c = &mca_debugger_mpirx_component.base_version;

    mca_base_param_reg_int(c, "check_rate",
                           "Set rate (in secs) for auto-detect of debugger attachment (0 => do not check)",
                           false, false, 0, &orte_debugger_mpirx_check_rate);
    return ORTE_SUCCESS;
}

static int component_query(mca_base_module_t **module, int *priority)
{
    *priority = 10;
    *module = (mca_base_module_t *)&orte_debugger_mpirx_module;
    return ORTE_SUCCESS;
}
