/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "rmcast_tcp.h"

/*
 * Local functions
 */
static int orte_rmcast_tcp_open(void);
static int orte_rmcast_tcp_close(void);
static int orte_rmcast_tcp_query(mca_base_module_t **module, int *priority);

/*
 * Local variables
 */
static bool initialized = false;

/*
 * Public string showing the iof hnp component version number
 */
const char *mca_rmcast_tcp_component_version_string =
    "Open MPI tcp rmcast MCA component version " ORTE_VERSION;

orte_rmcast_base_component_t mca_rmcast_tcp_component = {
    {
        ORTE_RMCAST_BASE_VERSION_1_0_0,
        
        "tcp", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        
        /* Component open, close, and query functions */
        orte_rmcast_tcp_open,
        orte_rmcast_tcp_close,
        orte_rmcast_tcp_query 
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/**
  * component open/close/init function
  */
static int orte_rmcast_tcp_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_rmcast_tcp_close(void)
{
    return ORTE_SUCCESS;
}

/**
 * Module query
 */

static int orte_rmcast_tcp_query(mca_base_module_t **module, int *priority)
{
    if (!ORTE_PROC_IS_HNP && NULL == orte_process_info.my_hnp_uri) {
        /* cannot operate */
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* selected by choice */
    *priority = 0;
    *module = (mca_base_module_t *) &orte_rmcast_tcp_module;
    initialized = true;
    
    return ORTE_SUCCESS;
}
