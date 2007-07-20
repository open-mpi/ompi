/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "routed_unity.h"

#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

static orte_routed_module_t* routed_unity_init(int* priority);


/**
 * component definition
 */
orte_routed_component_t mca_routed_unity_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rml v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_ROUTED_BASE_VERSION_1_0_0,

        "unity", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          MCA_BASE_METADATA_PARAM_NONE
      },
      routed_unity_init
};

orte_routed_module_t orte_routed_unity_module = {
        orte_routed_unity_finalize,

        orte_routed_unity_update_route,
        orte_routed_unity_get_route
};

static orte_routed_module_t*
routed_unity_init(int* priority)
{
    *priority = 10;

    return &orte_routed_unity_module;
}


int
orte_routed_unity_finalize(void)
{
    return ORTE_SUCCESS;
}


int
orte_routed_unity_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_unity_update: %s --> %s",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));
    return ORTE_SUCCESS;
}


orte_process_name_t
orte_routed_unity_get_route(orte_process_name_t *target)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_unity_get(%s) --> %s",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(target)));
    return *target;
}
