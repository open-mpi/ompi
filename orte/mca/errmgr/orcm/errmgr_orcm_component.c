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
#include "errmgr_orcm.h"

/*
 * Public string for version number
 */
const char *orte_errmgr_orcm_component_version_string = 
    "ORTE ERRMGR orcm MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int errmgr_orcm_open(void);
static int errmgr_orcm_close(void);
static int errmgr_orcm_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_errmgr_base_component_t mca_errmgr_orcm_component =
{
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itorcm
     */
    {
        ORTE_ERRMGR_BASE_VERSION_3_0_0,
        /* Component name and version */
        "orcm",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        errmgr_orcm_open,
        errmgr_orcm_close,
        errmgr_orcm_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Verbosity level */
    0,
    /* opal_output handler */
    -1,
    /* Default priority */
    1
};

static int errmgr_orcm_open(void) 
{
    return ORTE_SUCCESS;
}

static int errmgr_orcm_close(void)
{
    return ORTE_SUCCESS;
}

static int errmgr_orcm_component_query(mca_base_module_t **module, int *priority)
{
    /*
     * This component is selected only when requested - and if so, then
     * it MUST be used exclusively
     */
    bool is_required = false;

    mca_base_is_component_required(&orte_errmgr_base_components_available,
                                   &mca_errmgr_orcm_component.base_version,
                                   true,
                                   &is_required);
    
    if( !is_required ) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    
    *priority = 1000;
    *module = (mca_base_module_t *)&orte_errmgr_orcm_module;
    return ORTE_SUCCESS;
}

