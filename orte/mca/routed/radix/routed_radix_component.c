/*
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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
#include "opal/util/sys_limits.h"

#include "orte/mca/routed/base/base.h"
#include "routed_radix.h"

static int orte_routed_radix_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_radix_component_t mca_routed_radix_component = {
    {
        /* First, the mca_base_component_t struct containing meta
        information about the component itself */

        {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "radix", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_radix_component_query
        },
        {
        /* This component can be checkpointed */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int orte_routed_radix_component_query(mca_base_module_t **module, int *priority)
{
    mca_base_component_t *c = &mca_routed_radix_component.super.base_version;
    int tmp;

    mca_base_param_reg_int(c, NULL,
                           "Radix to be used for routed radix tree",
                           false, false, 32, &mca_routed_radix_component.radix);
    mca_base_param_reg_int(c, "max_connections",
                           "Max number of connections a daemon may make before routing messages across tree",
                           false, false, -1, &tmp);
    if (0 < tmp) {
        mca_routed_radix_component.max_connections = tmp;
    } else if (0 < opal_sys_limits.num_files) {
        /* we really should compute the max connections as the total limit on file
         * descriptors minus the radix minus the fd's needed for our local
         * children. However, we don't have all that info until later, so just
         * take a reasonable approximation here
         */
        mca_routed_radix_component.max_connections = opal_sys_limits.num_files - mca_routed_radix_component.radix;
    } else {
        /* default to radix size for lack of anything better */
            mca_routed_radix_component.max_connections = mca_routed_radix_component.radix;
    }

    *priority = 30;
    *module = (mca_base_module_t *) &orte_routed_radix_module;
    return ORTE_SUCCESS;
}
