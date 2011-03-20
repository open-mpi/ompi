/* -*- C -*-
*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
*
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"


#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rmcast/rmcast.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_mcast.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_mcast_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "mcast", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_mcast_open,  /* module open */
        orte_grpcomm_mcast_close, /* module close */
        orte_grpcomm_mcast_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/* Open the component */
int orte_grpcomm_mcast_open(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_mcast_close(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_mcast_component_query(mca_base_module_t **module, int *priority)
{
    /* This component is selected only when requested */
    bool is_required = false;
    
    mca_base_is_component_required(&orte_grpcomm_base.components_available,
                                   &mca_grpcomm_mcast_component.base_version,
                                   true,
                                   &is_required);
    
    if( !is_required ) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    if (NULL == orte_rmcast.recv_buffer_nb) {
        /* check any API to ensure mcast support was built */
        opal_output(0, "MCAST GRPCOMM WAS SPECIFIED, BUT MULTICAST SUPPORT WAS NOT BUILT");
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    *priority = 1000;
    *module = (mca_base_module_t *)&orte_grpcomm_mcast_module;
    return ORTE_SUCCESS;
}
