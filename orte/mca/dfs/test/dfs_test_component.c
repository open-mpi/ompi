/*
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/dfs/base/base.h"
#include "dfs_test.h"

/*
 * Public string for version number
 */
const char *orte_dfs_test_component_version_string = 
    "ORTE DFS test MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int dfs_test_open(void);
static int dfs_test_close(void);
static int dfs_test_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_dfs_base_component_t mca_dfs_test_component =
{
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component
     */
    {
        ORTE_DFS_BASE_VERSION_1_0_0,
        /* Component name and version */
        "test",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        dfs_test_open,
        dfs_test_close,
        dfs_test_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static bool select_me = false;

static int dfs_test_open(void) 
{
    int tmp;
    mca_base_component_t *c = &mca_dfs_test_component.base_version;
    mca_base_param_reg_int(c, "select",
                           "Apps select the test plug-in for the DFS framework",
                           false, false, (int)false, &tmp);
    select_me = OPAL_INT_TO_BOOL(tmp);
    return ORTE_SUCCESS;
}

static int dfs_test_close(void)
{
    return ORTE_SUCCESS;
}

static int dfs_test_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_APP && select_me) {
        /* set our priority high so apps use us */
        *priority = 10000;
        *module = (mca_base_module_t *)&orte_dfs_test_module;
        return ORTE_SUCCESS;        
    }
    
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
