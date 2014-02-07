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

    /* JMS Artifically low for now */
    module_priority = 0;
    (void) mca_base_component_var_register (c, "priority", "Priority of the LAMA rmaps component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &module_priority);

    rmaps_lama_timing_enabled = false;
    (void) mca_base_component_var_register (c, "timing",
                                            "Enable timing information. [Default = disabled]",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &rmaps_lama_timing_enabled);

    rmaps_lama_cmd_map = NULL;
    (void) mca_base_component_var_register (c, "map", "LAMA Map: Process layout iteration ordering (See documentation)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &rmaps_lama_cmd_map);

    rmaps_lama_cmd_bind = NULL;
    (void) mca_base_component_var_register (c, "bind", "LAMA Bind: Bind to the specified number of resources (See documentation)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &rmaps_lama_cmd_bind);

    rmaps_lama_cmd_mppr = NULL;
    (void) mca_base_component_var_register (c, "mppr", "LAMA MPPR: Maximum number of the specified resources available (See documentation)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &rmaps_lama_cmd_mppr);

    rmaps_lama_cmd_ordering = NULL;
    (void) mca_base_component_var_register (c, "ordering", "LAMA Ordering: Ordering (s) sequential, (n) natural - Default: n (See documentation)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &rmaps_lama_cmd_ordering);

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Priority %3d",
                        module_priority);

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Map   : %s",
                        (NULL == rmaps_lama_cmd_map) ? "NULL" : rmaps_lama_cmd_map);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Bind  : %s",
                        (NULL == rmaps_lama_cmd_bind) ? "NULL" : rmaps_lama_cmd_bind);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: MPPR  : %s",
                        (NULL == rmaps_lama_cmd_mppr) ? "NULL" : rmaps_lama_cmd_mppr);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Order : %s",
                        (NULL == rmaps_lama_cmd_ordering) ? "NULL" : rmaps_lama_cmd_ordering);

    return ORTE_SUCCESS;
}


static int orte_rmaps_lama_query(mca_base_module_t **module, int *priority)
{
    /* Only run on the HNP */

    *priority = module_priority;
    *module = (mca_base_module_t *)&orte_rmaps_lama_module;

    return ORTE_SUCCESS;
}
