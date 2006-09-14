/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmgr/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmgr_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rmgr_base_component_t *component, *best_component = NULL;
    orte_rmgr_base_module_t *module, *best_module = NULL;
    int priority, best_priority = -1;

    OPAL_TRACE(5);
    
    /* Iterate through all the available components */
    for (item = opal_list_get_first(&orte_rmgr_base.rmgr_components);
         item != opal_list_get_end(&orte_rmgr_base.rmgr_components);
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rmgr_base_component_t *) cli->cli_component;

        /* Call the component's init function and see if it wants to be selected */
        module = component->rmgr_init(&priority);

        /* If we got a non-NULL module back, then the component wants to
           be selected.  So save its multi/hidden values and save the
           module with the highest priority */

        if (NULL == module)
            continue;

        /* If this is the best one, save it */

        if (priority > best_priority) {

            /* If there was a previous best one, finalize */

            if (NULL != best_module && NULL != best_module->finalize) {
                 best_module->finalize();
            }

            /* Save the new best one */
            best_module = module;
            best_component = component;

            /* update the best priority */
            best_priority = priority;
        } 
    }

    /* If we didn't find one to select, barf */
    if (NULL == best_module) {
        opal_output(orte_rmgr_base.rmgr_output,
                    "rmgr:select: no components available!");
        return ORTE_ERROR;
    }

    /* save the module for later usage */
    orte_rmgr = *best_module;
    
    /* let the module do it's own init, if needed */
    if (NULL != orte_rmgr.module_init) {
        orte_rmgr.module_init();
    }
    return ORTE_SUCCESS;
}

