/* -*- C -*-
*
* Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/

#include "orte_config.h"
#include "orte/constants.h"

#include <pmi.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"


#include "grpcomm_pmi.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_pmi_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "pmi", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_pmi_open,  /* module open */
        orte_grpcomm_pmi_close, /* module close */
        orte_grpcomm_pmi_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/* Open the component */
int orte_grpcomm_pmi_open(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_close(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_component_query(mca_base_module_t **module, int *priority)
{
    int spawned;
    /* if we can't startup the PMI, we can't be used */
    if (PMI_SUCCESS != PMI_Init(&spawned)) {
        *priority = -1;
        *module = NULL;
        return ORTE_ERROR;
    }
    /* we are a default, so set a low priority so we can be overridden */
    *priority = 1;
    *module = (mca_base_module_t *)&orte_grpcomm_pmi_module;
    return ORTE_SUCCESS;    
}
