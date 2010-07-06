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

#include "mpir.h"


static int component_query(mca_base_module_t **module, int *priority);


/*
 * Struct of function pointers that need to be initialized
 */
orte_debugger_base_component_t mca_debugger_mpir_component = {
    {
        ORTE_DEBUGGER_BASE_VERSION_1_0_0,
        
        "mpir", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */

        NULL,
        NULL,
        component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int component_query(mca_base_module_t **module, int *priority)
{
    *priority = 100;
    *module = (mca_base_module_t *)&orte_debugger_mpir_module;
    return ORTE_SUCCESS;
}
