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

#include "opal/mca/base/base.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"

#include "orte/mca/iof/base/base.h"
#include "iof_mrhnp.h"

/*
 * Local functions
 */
static int mrhnp_open(void);
static int mrhnp_close(void);
static int mrhnp_query(mca_base_module_t **module, int *priority);

/*
 * Public string showing the iof hnp component version number
 */
const char *mca_iof_mr_hnp_component_version_string =
    "Open MPI mr_hnp iof MCA component version " ORTE_VERSION;

orte_iof_mrhnp_component_t mca_iof_mr_hnp_component = {
    {
        /* First, the mca_base_component_t struct containing meta
         information about the component itself */
        
        {
            ORTE_IOF_BASE_VERSION_2_0_0,
            
            "mr_hnp", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            
            /* Component open, close, and query functions */
            mrhnp_open,
            mrhnp_close,
            mrhnp_query 
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        
    }
};

/**
  * component open/close/init function
  */
static int mrhnp_open(void)
{
    return ORTE_SUCCESS;
}


static int mrhnp_close(void)
{
    return ORTE_SUCCESS;
}

/**
 * Module query
 */

static int mrhnp_query(mca_base_module_t **module, int *priority)
{
    mca_iof_mr_hnp_component.input_files = NULL;

    /* select if we are HNP and map-reduce mode is operational */
    if (ORTE_PROC_IS_HNP && orte_map_reduce) {
        *priority = 1000;
        *module = (mca_base_module_t *) &orte_iof_mrhnp_module;
        if (NULL != orte_iof_base.input_files) {
            mca_iof_mr_hnp_component.input_files = opal_argv_split(orte_iof_base.input_files, ',');
        }
        return ORTE_SUCCESS;
    }
        
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
