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
static int state_novm_register (void);
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
        state_novm_component_query,
        state_novm_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static bool select_me = false;

static int state_novm_register (void)
{
    int ret;

    select_me = false;
    ret = mca_base_component_var_register (&mca_state_novm_component.base_version,
                                           "select", "Use this component",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL,
                                           0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &select_me);
    return (0 > ret) ? ret : ORTE_SUCCESS;
}

static int state_novm_open(void) 
{
    return ORTE_SUCCESS;
}

static int state_novm_close(void)
{
    return ORTE_SUCCESS;
}

static int state_novm_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_HNP && select_me) {
        /* set our priority high so we'll be selected if user desires */
        *priority = 1000;
        *module = (mca_base_module_t *)&orte_state_novm_module;
        return ORTE_SUCCESS;        
    }
    
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
