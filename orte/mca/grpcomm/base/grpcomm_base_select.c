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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/grpcomm/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_grpcomm_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_grpcomm_base_component_t *component, *best_component = NULL;
    orte_grpcomm_base_module_t *module, *best_module = NULL;
    int priority, best_priority = -1;
    
    /* Iterate through all the available components */
    
    for (item = opal_list_get_first(&mca_grpcomm_base_components_available);
         item != opal_list_get_end(&mca_grpcomm_base_components_available);
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_grpcomm_base_component_t *) cli->cli_component;
        
        /* Call the component's init function and see if it wants to be
         * selected
         */
        
        module = component->grpcomm_init(&priority);
        
        /* If we got a non-NULL module back, then the component wants to
         * be selected.  So save the module with the highest priority
         */
        
        if (NULL != module) {
            /* If this is the best one, save it */
            
            if (priority > best_priority) {
                
                /* Save the new best one */
                
                best_module = module;
                best_component = component;
                
                /* update the best priority */
                best_priority = priority;
            } 
            
        }
    }
    
    /* If we didn't find one to select, barf */
    
    if (NULL == best_component) {
        return ORTE_ERROR;
    }
    
    /* We have happiness */
    orte_grpcomm = *best_module;
    if (ORTE_SUCCESS != orte_grpcomm.init()) {
        /* ouch! */
        return ORTE_ERROR;
    }
    mca_grpcomm_base_selected = true;
    
    /* all done */
    
    return ORTE_SUCCESS;
}
