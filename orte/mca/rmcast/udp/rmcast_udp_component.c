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
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/base/base.h"
#include "rmcast_udp.h"

/*
 * Local functions
 */
static int orte_rmcast_udp_open(void);
static int orte_rmcast_udp_close(void);
static int orte_rmcast_udp_query(mca_base_module_t **module, int *priority);

/*
 * Local variables
 */
static bool initialized = false;
int orte_rmcast_udp_sndbuf_size;
int orte_rmcast_udp_rcvbuf_size;

/*
 * Public string showing the rmcast udp component version number
 */
const char *mca_rmcast_udp_component_version_string =
    "Open MPI udp rmcast MCA component version " ORTE_VERSION;

orte_rmcast_base_component_t mca_rmcast_udp_component = {
    {
        ORTE_RMCAST_BASE_VERSION_1_0_0,
        
        "udp", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        
        /* Component open, close, and query functions */
        orte_rmcast_udp_open,
        orte_rmcast_udp_close,
        orte_rmcast_udp_query 
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/**
  * component open/close/init function
  */
static int orte_rmcast_udp_open(void)
{
    mca_base_component_t *c = &mca_rmcast_udp_component.version;
    int value;
    
    mca_base_param_reg_int(c, "sndbuf_size", 
                           "Size of send buffer in Kbytes (must be > 0)", 
                           false, false, 
                           ORTE_RMCAST_UDP_DEFAULT_SNDBUF_SIZE, &value);
    if (ORTE_RMCAST_UDP_DEFAULT_SNDBUF_SIZE != value) {
        orte_rmcast_udp_sndbuf_size = 1024*value;
    } else {
        orte_rmcast_udp_sndbuf_size = ORTE_RMCAST_UDP_DEFAULT_SNDBUF_SIZE;
    }

    
    orte_rmcast_udp_rcvbuf_size = 16 * orte_rmcast_udp_sndbuf_size;
    mca_base_param_reg_int(c, "rcvbuf_size", 
                           "Size of recv buffer in Kbytes (default: 16xsndbuf)", 
                           false, false, 
                           orte_rmcast_udp_rcvbuf_size, &value);
    if (value != orte_rmcast_udp_rcvbuf_size) {
        orte_rmcast_udp_rcvbuf_size = 1024 * value;
    }

    return ORTE_SUCCESS;
}


static int orte_rmcast_udp_close(void)
{
    return ORTE_SUCCESS;
}

/**
 * Module query
 */

static int orte_rmcast_udp_query(mca_base_module_t **module, int *priority)
{
    /* selected by default */
    *priority = 100;
    *module = (mca_base_module_t *) &orte_rmcast_udp_module;
    initialized = true;
    
    return ORTE_SUCCESS;
}
