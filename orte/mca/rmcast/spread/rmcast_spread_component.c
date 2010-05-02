/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/rmcast/rmcast.h"
#include "rmcast_spread.h"

extern orte_rmcast_module_t orte_rmcast_spread_module;

/*
 * Public string showing the rmcast spread component version number
 */
const char *mca_rmcast_spread_component_version_string =
    "Open MPI spread rmcast MCA component version " ORTE_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_rmcast_spread_component_t mca_rmcast_spread_component = {
    {
        {
            ORTE_RMCAST_BASE_VERSION_1_0_0,
            
            /* Component name and version */
            "spread",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            orte_rmcast_spread_component_open,
            orte_rmcast_spread_component_close,
            orte_rmcast_spread_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


int
orte_rmcast_spread_component_open(void)
{
    mca_base_component_t *c = &mca_rmcast_spread_component.super.version;
    
    mca_base_param_reg_int(c, "max_msg_size",
                           "Max #bytes in a single msg (must be > 0)",
                           false, false,
                           ORTE_RMCAST_SPREAD_MAX_MSG_SIZE,
                           &mca_rmcast_spread_component.max_msg_size);

    return ORTE_SUCCESS;
}


int orte_rmcast_spread_component_query(mca_base_module_t **module, int *priority)
{
    /* if we built, then we probably want to be selected */
    *priority = 1000;
    *module = (mca_base_module_t*)&orte_rmcast_spread_module;
    return ORTE_SUCCESS;
}


int
orte_rmcast_spread_component_close(void)
{
    return ORTE_SUCCESS;
}

