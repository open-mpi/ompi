/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"



#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "filem_raw.h"

/*
 * Public string for version number
 */
const char *orte_filem_raw_component_version_string = 
"ORTE FILEM raw MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int filem_raw_open(void);
static int filem_raw_close(void);
static int filem_raw_query(mca_base_module_t **module, int *priority);

orte_filem_base_component_t mca_filem_raw_component = {
    {
        ORTE_FILEM_BASE_VERSION_2_0_0,
        /* Component name and version */
        "raw",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
            
        /* Component open and close functions */
        filem_raw_open,
        filem_raw_close,
        filem_raw_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static int filem_raw_open(void) 
{
    return ORTE_SUCCESS;
}

static int filem_raw_close(void)
{
    return ORTE_SUCCESS;
}

static int filem_raw_query(mca_base_module_t **module, int *priority)
{
    *priority = 0;

    /* only select when requested, and never for an APP */
    if (ORTE_PROC_IS_APP) {
        *module = NULL;
        return ORTE_ERROR;
    }

#if ORTE_WANT_HADOOP_SUPPORT
    /* always use us if Hadoop is enabled */
    *priority = 1000;
#endif

    *module = (mca_base_module_t*) &mca_filem_raw_module;
    return ORTE_SUCCESS;
}
