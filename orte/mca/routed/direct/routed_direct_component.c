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
#include "orte/constants.h"


#include "routed_direct.h"

static orte_routed_module_t* routed_direct_init(int* priority);


/**
 * component definition
 */
orte_routed_component_t mca_routed_direct_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rml v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_ROUTED_BASE_VERSION_1_0_0,

        "direct", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },
      routed_direct_init
};

static orte_routed_module_t*
routed_direct_init(int* priority)
{
    *priority = 10;

    return &orte_routed_direct_module;
}
