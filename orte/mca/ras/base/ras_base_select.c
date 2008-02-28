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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/base/base.h"


/*
 * Select one RAS component from all those that are available.
 */
int orte_ras_base_select(void)
{
#ifdef ORTE_WANT_NO_RAS_SUPPORT
    /* some systems require no allocation support - they handle
    * the allocation internally themselves. In those cases, memory
    * footprint is often a consideration. Hence, we provide a means
    * for someone to transparently configure out all RAS support.
    */
    return ORTE_SUCCESS;
    
#else
    /* For all other systems, provide the following support */
    
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_ras_base_component_t *component, *best_component=NULL;
    orte_ras_base_module_t *module, *best_module=NULL;
    int priority, best_priority = -1;

    /* Query all the opened components and see if they want to be selected */
    for (item = opal_list_get_first(&orte_ras_base.ras_opened);
         opal_list_get_end(&orte_ras_base.ras_opened) != item;
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_ras_base_component_t *) cli->cli_component;

        OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                             "%s ras:base:select querying component %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             component->ras_version.mca_component_name));
        
        /* Call the component's init function and see if it wants to be
           selected */

        module = component->ras_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:select component %s returns priority %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 component->ras_version.mca_component_name,
                                 priority));
            
            /* If this is the best one, save it */
            if (priority > best_priority) {
                
                /* If there was a previous best one, finalize */
                if (NULL != best_module) {
                    best_module->finalize();
                }
                
                /* Save the new best one */
                best_module = module;
                best_component = component;
                
                /* update the best priority */
                best_priority = priority;
            } else {
                OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                     "%s ras:base:select component %s does did not win the election",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     component->ras_version.mca_component_name));
                if (NULL == module->finalize) {
                    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                         "%s ras:base:select It appears you are the victim of a stale library - please delete your installation lib directory and reinstall",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                } else {
                    module->finalize();
                }
            }
        } else {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:select component %s does NOT want to be considered for selection",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 component->ras_version.mca_component_name));
            }
        }

    /* If we didn't find one to select, that is okay */

    if (NULL == best_component) {
        return ORTE_SUCCESS;
    }

    /* We have happiness */
    orte_ras_base.active_module = best_module;

    return ORTE_SUCCESS;
#endif
}
