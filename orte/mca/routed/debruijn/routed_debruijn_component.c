/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
#include "routed_debruijn.h"

static int orte_routed_debruijn_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_component_t mca_routed_debruijn_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "debruijn", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_debruijn_component_query
      },
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
};

static int orte_routed_debruijn_component_query(mca_base_module_t **module, int *priority)
{
    /* Debruijn shall be our default, especially for large systems. For smaller
     * systems, we will allow other options that have even fewer hops to
     * support wireup
     *
     * XXX: set this to 0 until we can figure out what's going on with
     * it within undersubscribed allocations. Once debruijn is fixed,
     * revert back to priority 70. Note: this component seems to work fine within
     * fully utilized allocations.
     */
    *priority = 0;
    *module = (mca_base_module_t *) &orte_routed_debruijn_module;
    return ORTE_SUCCESS;
}
