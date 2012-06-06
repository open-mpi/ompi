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


#include "orte/mca/routed/base/base.h"
#include "routed_radix2.h"

static int orte_routed_radix2_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_radix2_component_t mca_routed_radix2_component = {
    {
        /* First, the mca_base_component_t struct containing meta
        information about the component itself */

        {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "radix2", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_radix2_component_query
        },
        {
        /* This component can be checkpointed */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int orte_routed_radix2_component_query(mca_base_module_t **module, int *priority)
{
    mca_base_component_t *c = &mca_routed_radix2_component.super.base_version;

    mca_base_param_reg_int(c, NULL,
                           "Radix2 to be used for routed radix2 tree",
                           false, false, 32, &mca_routed_radix2_component.radix);
    *priority = 65;
    *module = (mca_base_module_t *) &orte_routed_radix2_module;
    return ORTE_SUCCESS;
}
