/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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
#include "state_novm.h"

/*
 * Public string for version number
 */
const char *orte_state_novm_component_version_string = 
    "ORTE STATE novm MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int state_novm_open(void);
static int state_novm_close(void);
static int state_novm_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_state_base_component_t mca_state_novm_component =
{
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component
     */
    {
        ORTE_STATE_BASE_VERSION_1_0_0,
        /* Component name and version */
        "novm",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        state_novm_open,
        state_novm_close,
        state_novm_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static int my_priority;

static int state_novm_open(void) 
{
    mca_base_component_t *c=&mca_state_novm_component.base_version;

    mca_base_param_reg_int(c, "priority",
                           "Selection priority",
                           false, false, 50, &my_priority);
    return ORTE_SUCCESS;
}

static int state_novm_close(void)
{
    return ORTE_SUCCESS;
}

static int state_novm_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_HNP) {
        /* set our priority mid-range so we'll be selected if user desires */
        *priority = my_priority;
        *module = (mca_base_module_t *)&orte_state_novm_module;
        return ORTE_SUCCESS;        
    }
    
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
