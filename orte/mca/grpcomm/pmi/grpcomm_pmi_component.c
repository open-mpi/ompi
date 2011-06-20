/* -*- C -*-
*
* Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/
/** @file:
*
* The Open MPI Name Server
*
* The Open MPI Name Server provides unique name ranges for processes
* within the universe. Each universe will have one name server
* running within the seed daemon.  This is done to prevent the
* inadvertent duplication of names.
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"


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
    /*** see if we can run */
    if (NULL == getenv("SLURM_JOBID")) {
        *module = NULL;
        return ORTE_ERROR;
    }

    /* we are a default, so set a low priority so we can be overridden */
    *priority = 10;
    *module = (mca_base_module_t *)&orte_grpcomm_pmi_module;
    return ORTE_SUCCESS;    
}
