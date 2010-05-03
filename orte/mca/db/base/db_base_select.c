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

#include "orte/mca/db/base/base.h"

extern opal_list_t orte_db_base_components_available;

int 
orte_db_base_select(void)
{
    orte_db_base_component_t *best_component = NULL;
    orte_db_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("db", orte_db_base_output,
                                        &orte_db_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay to not select a component - default
         * to using the base NULL component
         */
        return ORTE_SUCCESS;
    }

    /* Save and init the winner */
    orte_db = *best_module;
    if (NULL != orte_db.init) {
        orte_db.init();
    }

    return ORTE_SUCCESS;
}
