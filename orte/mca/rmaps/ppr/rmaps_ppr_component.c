/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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
#include "rmaps_ppr.h"

/*
 * Local functions
 */

static int orte_rmaps_ppr_open(void);
static int orte_rmaps_ppr_close(void);
static int orte_rmaps_ppr_query(mca_base_module_t **module, int *priority);
static int orte_rmaps_ppr_register(void);

orte_rmaps_base_component_t mca_rmaps_ppr_component = {
    {
        ORTE_RMAPS_BASE_VERSION_2_0_0,
        
        "ppr", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_ppr_open,  /* component open  */
        orte_rmaps_ppr_close, /* component close */
        orte_rmaps_ppr_query, /* component query */
        orte_rmaps_ppr_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int my_priority;

static int orte_rmaps_ppr_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_rmaps_ppr_query(mca_base_module_t **module, int *priority)
{
    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_rmaps_ppr_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_ppr_close(void)
{
    return ORTE_SUCCESS;
}


static int orte_rmaps_ppr_register(void)
{
    my_priority = 90;
    (void) mca_base_component_var_register(&mca_rmaps_ppr_component.base_version,
                                           "priority", "Priority of the ppr rmaps component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &my_priority);

    return ORTE_SUCCESS;
}
