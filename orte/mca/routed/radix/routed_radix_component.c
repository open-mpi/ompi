/*
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "orte/mca/routed/base/base.h"
#include "routed_radix.h"

static int orte_routed_radix_component_register(void);
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
        orte_routed_radix_component_query,
        orte_routed_radix_component_register
        },
        {
        /* This component can be checkpointed */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int orte_routed_radix_component_register(void)
{
    mca_base_component_t *c = &mca_routed_radix_component.super.base_version;

    mca_routed_radix_component.radix = 32;
    (void) mca_base_component_var_register(c, NULL,
                                           "Radix to be used for routed radix tree",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_routed_radix_component.radix);

    mca_routed_radix_component.max_connections = -1;
    (void) mca_base_component_var_register(c, "max_connections",
                                           "Send direct between daemons if the number of nodes is less than this number",
                                           MCA_BASE_VAR_TYPE_INT, NULL,0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_routed_radix_component.max_connections);

    return ORTE_SUCCESS;
}

static int orte_routed_radix_component_query(mca_base_module_t **module, int *priority)
{
    if (0 > mca_routed_radix_component.radix) {
        return ORTE_ERR_BAD_PARAM;
    }

    *priority = 30;
    *module = (mca_base_module_t *) &orte_routed_radix_module;
    return ORTE_SUCCESS;
}
