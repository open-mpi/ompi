/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "state_staged_hnp.h"

/*
 * Public string for version number
 */
const char *orte_state_staged_hnp_component_version_string = 
    "ORTE STATE staged_hnp MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int state_staged_hnp_open(void);
static int state_staged_hnp_close(void);
static int state_staged_hnp_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_state_base_component_t mca_state_staged_hnp_component =
{
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component
     */
    {
        ORTE_STATE_BASE_VERSION_1_0_0,
        /* Component name and version */
        "staged_hnp",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        state_staged_hnp_open,
        state_staged_hnp_close,
        state_staged_hnp_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static int state_staged_hnp_open(void) 
{
    return ORTE_SUCCESS;
}

static int state_staged_hnp_close(void)
{
    return ORTE_SUCCESS;
}

static int state_staged_hnp_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_HNP && orte_staged_execution) {
        *priority = 1000;
        *module = (mca_base_module_t *)&orte_state_staged_hnp_module;
        return ORTE_SUCCESS;        
    }
    
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
