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
#include "orte/constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/base/base.h"


/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmaps_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rmaps_base_component_t *component, *best_component=NULL;
    orte_rmaps_base_module_t *module, *best_module=NULL;
    int priority, best_priority = -1;
    
    /* Query all the available components and see if they want to run */
    for (item = opal_list_get_first(&orte_rmaps_base.available_components); 
         opal_list_get_end(&orte_rmaps_base.available_components) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rmaps_base_component_t *) cli->cli_component;

        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                             "%s orte:rmaps:base:select: querying component %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             component->rmaps_version.mca_component_name));

        /* Call the component's init function and see if it wants to be
            selected */
        
        module = component->rmaps_init(&priority);
        
        /* If we got a non-NULL module back, then the component wants
            to be considered for selection */
        
        if (NULL != module) {
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s orte:rmaps:base:select: component %s returns priority %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 component->rmaps_version.mca_component_name,
                                 priority));
            
            /* If this is the best one, save it */
            if (priority > best_priority) {
                /* Save the new best one */
                best_module = module;
                best_component = component;
                
                /* update the best priority */
                best_priority = priority;
            } else {
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s orte:rmaps:base:select: component %s does did not win the election",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     component->rmaps_version.mca_component_name));
            }
        } else {
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s orte:rmaps:base:select: component %s does NOT want to be considered for selection",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 component->rmaps_version.mca_component_name));
        }
    }
    
    /* If we didn't find one to select, that is fatal */
    
    if (NULL == best_component) {
        return ORTE_ERROR;
    }
    
    /* We have happiness */
    orte_rmaps_base.active_module = best_module;

    /* all done */
    return ORTE_SUCCESS;
}
