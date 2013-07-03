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

#include "orte/util/proc_info.h"

#include "iof_mrorted.h"

/*
 * Local functions
 */
static int mr_orted_open(void);
static int mr_orted_close(void);
static int mr_orted_query(mca_base_module_t **module, int *priority);


/*
 * Public string showing the iof mr_orted component version number
 */
const char *mca_iof_mr_orted_component_version_string =
"Open MPI mr_orted iof MCA component version " ORTE_VERSION;


orte_iof_mrorted_component_t mca_iof_mr_orted_component = {
    {
        {
            ORTE_IOF_BASE_VERSION_2_0_0,
            
            "mr_orted", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            
            /* Component open, close, and query functions */
            mr_orted_open,
            mr_orted_close,
            mr_orted_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

/**
  * component open/close/init function
  */
static int mr_orted_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}

static int mr_orted_close(void)
{
    return ORTE_SUCCESS;
}


static int mr_orted_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_DAEMON && orte_map_reduce) {
        *priority = 1000;
        *module = (mca_base_module_t *) &orte_iof_mrorted_module;
        return ORTE_SUCCESS;
    }
        
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}

