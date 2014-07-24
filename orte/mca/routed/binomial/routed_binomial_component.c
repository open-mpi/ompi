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

#include "orte/mca/routed/base/base.h"
#include "routed_binomial.h"

static int orte_routed_binomial_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_component_t mca_routed_binomial_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "binomial", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_binomial_component_query
      },
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
};

static int orte_routed_binomial_component_query(mca_base_module_t **module, int *priority)
{
    /* make this selected ONLY if the user directs as this module scales
     * poorly compared to our other options
     */
    *priority = 30;
    *module = (mca_base_module_t *) &orte_routed_binomial_module;
    return ORTE_SUCCESS;
}
