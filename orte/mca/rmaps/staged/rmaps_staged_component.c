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
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/base.h"
#include "rmaps_staged.h"

/*
 * Local functions
 */

static int orte_rmaps_staged_open(void);
static int orte_rmaps_staged_close(void);
static int orte_rmaps_staged_query(mca_base_module_t **module, int *priority);

orte_rmaps_base_component_t mca_rmaps_staged_component = {
    {
        ORTE_RMAPS_BASE_VERSION_2_0_0,
        
        "staged", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_staged_open,  /* component open  */
        orte_rmaps_staged_close, /* component close */
        orte_rmaps_staged_query  /* component query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_staged_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_rmaps_staged_query(mca_base_module_t **module, int *priority)
{
    *priority = 5;
    *module = (mca_base_module_t *)&orte_rmaps_staged_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_staged_close(void)
{
    return ORTE_SUCCESS;
}


