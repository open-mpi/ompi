/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved. 
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

#include "orte/util/proc_info.h"

#include "orte/mca/routed/base/base.h"
#include "routed_hdmon.h"

static int hdmon_open(void);
static int hdmon_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_hdmon_component_t mca_routed_hdmon_component = {
    {
        /* First, the mca_base_component_t struct containing meta
        information about the component itself */

        {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "hdmon", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        hdmon_open,
        NULL,
        hdmon_query
        },
        {
        /* This component can be checkpointed */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int hdmon_open(void)
{
    mca_base_component_t *c = &mca_routed_hdmon_component.super.base_version;

    mca_base_param_reg_int(c, "radix",
                           "Radix to be used for routed hdmon tree",
                           false, false, 32, &mca_routed_hdmon_component.radix);

    return ORTE_SUCCESS;
}

static int hdmon_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_PROC_IS_CM || ORTE_PROC_IS_CMSLAVE) {
        *priority = 1000;
        *module = (mca_base_module_t *) &orte_routed_hdmon_module;
        return ORTE_SUCCESS;
    }

    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}
