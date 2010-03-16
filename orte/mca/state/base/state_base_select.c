/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/mca/state/base/base.h"

extern opal_list_t orte_state_base_components_available;

int 
orte_state_base_select(void)
{
    orte_state_base_component_t *best_component = NULL;
    orte_state_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("state", orte_state_base_output,
                                        &orte_state_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay to not select a component - default
         * to using the base NULL component
         */
        return ORTE_SUCCESS;
    }

    /* Save and init the winner */
    orte_state = *best_module;
    if (NULL != orte_state.init) {
        orte_state.init();
    }

    return ORTE_SUCCESS;
}
