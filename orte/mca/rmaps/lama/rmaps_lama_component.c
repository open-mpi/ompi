/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 *
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

#include "rmaps_lama.h"

/*
 * Local functions
 */

static int orte_rmaps_lama_register(void);
static int orte_rmaps_lama_query(mca_base_module_t **module, int *priority);

static int module_priority;

char * rmaps_lama_cmd_map  = NULL;
char * rmaps_lama_cmd_bind = NULL;
char * rmaps_lama_cmd_mppr = NULL;
char * rmaps_lama_cmd_ordering = NULL;
bool rmaps_lama_timing_enabled = false;
bool rmaps_lama_can_oversubscribe = false;
bool rmaps_lama_am_oversubscribing = false;

orte_rmaps_base_component_t mca_rmaps_lama_component = {
    {
        ORTE_RMAPS_BASE_VERSION_2_0_0,
        
        "lama", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,  /* component open  */
        NULL,  /* component close */
        orte_rmaps_lama_query,  /* component query */
        orte_rmaps_lama_register  /* component register */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int orte_rmaps_lama_register(void)
{
    mca_base_component_t *c = &mca_rmaps_lama_component.base_version;
    int val;

    /* JJH: Note the inflated priority, fix before release */
    mca_base_param_reg_int(c, "priority",
                           "Priority of the LAMA rmaps component",
                           false, false, 5000,
                           &module_priority);

    mca_base_param_reg_int(c, "timing",
                           "Enable timing information. [Default = disabled]",
                           false, false, 0,
                           &val);
    rmaps_lama_timing_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_string(c, "map",
                              "LAMA Map: Process layout iteration ordering (See documentation)",
                              false, false, NULL,
                              &rmaps_lama_cmd_map);

    mca_base_param_reg_string(c, "bind",
                              "LAMA Bind: Bind to the specified number of resources (See documentation)",
                              false, false, NULL,
                              &rmaps_lama_cmd_bind);

    mca_base_param_reg_string(c, "mppr",
                              "LAMA MPPR: Maximum number of the specified resources available (See documentation)",
                              false, false, NULL,
                              &rmaps_lama_cmd_mppr);

    mca_base_param_reg_string(c, "ordering",
                              "LAMA Ordering: Ordering (s) sequential, (n) natural - Default: n (See documentation)",
                              false, false, NULL,
                              &rmaps_lama_cmd_ordering);

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:lama: Priority %3d",
                        module_priority);

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:lama: Map   : %s",
                        rmaps_lama_cmd_map);
    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:lama: Bind  : %s",
                        rmaps_lama_cmd_bind);
    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:lama: MPPR  : %s",
                        rmaps_lama_cmd_mppr);
    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:lama: Order : %s",
                        rmaps_lama_cmd_ordering);

    return ORTE_SUCCESS;
}


static int orte_rmaps_lama_query(mca_base_module_t **module, int *priority)
{
    /* Only ran on the HNP */

    *priority = module_priority;
    *module = (mca_base_module_t *)&orte_rmaps_lama_module;

    return ORTE_SUCCESS;
}
