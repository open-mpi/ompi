/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "errmgr_ortedresil.h"

/*
 * Public string for version number
 */
const char *orte_errmgr_ortedresil_component_version_string = 
    "ORTE ERRMGR ortedresil MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int errmgr_ortedresil_open(void);
static int errmgr_ortedresil_close(void);
static int errmgr_ortedresil_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_errmgr_base_component_t mca_errmgr_ortedresil_component =
{
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itortedresil
     */
    {
        ORTE_ERRMGR_BASE_VERSION_3_0_0,
        /* Component name and version */
        "ortedresil",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        errmgr_ortedresil_open,
        errmgr_ortedresil_close,
        errmgr_ortedresil_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int errmgr_ortedresil_open(void) 
{
    return ORTE_SUCCESS;
}

static int errmgr_ortedresil_close(void)
{
    return ORTE_SUCCESS;
}

static int errmgr_ortedresil_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_DAEMON) {
        /* keep our priority low so that other modules are higher
         * and will run before us
         */
        *priority = 0;
        *module = (mca_base_module_t *)&orte_errmgr_ortedresil_module;
        return ORTE_SUCCESS;        
    }
    
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}

