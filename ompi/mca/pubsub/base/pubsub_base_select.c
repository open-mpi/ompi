/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "ompi/mca/pubsub/pubsub.h"
#include "ompi/mca/pubsub/base/base.h"


int ompi_pubsub_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    ompi_pubsub_base_component_t *component, *best_component = NULL;
    ompi_pubsub_base_module_t *module, *best_module = NULL;
    int priority, best_priority = -1;
    int rc;
    
    /* Query all the opened components and see if they want to run */
    
    for (item = opal_list_get_first(&ompi_pubsub_base_components_available); 
         opal_list_get_end(&ompi_pubsub_base_components_available) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (ompi_pubsub_base_component_t *) cli->cli_component;
        
        OPAL_OUTPUT_VERBOSE((10, ompi_pubsub_base_output,
                             "ompi:pubsub:base:select: querying component %s",
                             component->pubsub_version.mca_component_name));
        
        /* Call the component's init function and see if it wants to be
            selected */
        
        module = component->pubsub_init(&priority);
        
        /* If we got a non-NULL module back, then the component wants
            to be considered for selection */
        
        if (NULL != module) {
            /* If this is the best one, save it */
            if (priority > best_priority) {
                
                /* If there was a previous best one, finalize */
                if (NULL != best_module) {
                    
                    OPAL_OUTPUT_VERBOSE((10, ompi_pubsub_base_output,
                                         "ompi:pubsub:base:select: found better component - finalizing component %s",
                                         best_component->pubsub_version.mca_component_name));
                    
                    best_module->finalize();
                }
                
                /* Save the new best one */
                best_module = module;
                best_component = component;
                
                /* update the best priority */
                best_priority = priority;
            } else {
                
                OPAL_OUTPUT_VERBOSE((10, ompi_pubsub_base_output,
                                     "ompi:pubsub:base:select: component %s does did not win the election",
                                     component->pubsub_version.mca_component_name));
                
                if (NULL == module->finalize) {
                    opal_output(ompi_pubsub_base_output,
                                "It appears you are the victim of a stale library - please delete your installation lib directory and reinstall");
                } else {
                    module->finalize();
                }
            }
        }
    }
    
    /* If we didn't find one to select, barf */
    
    if (NULL == best_component) {
        return OMPI_ERROR;
    }
    
    OPAL_OUTPUT_VERBOSE((10, ompi_pubsub_base_output,
                         "ompi:pubsub:base:select: component %s was selected",
                         best_component->pubsub_version.mca_component_name));

    /* We have happiness -- save the component and module for later
        usage */
    
    ompi_pubsub = *best_module;
    ompi_pubsub_base_selected_component = *best_component;
    
    /* unload all components that were not selected */
    item = opal_list_get_first(&ompi_pubsub_base_components_available);
    while(item != opal_list_get_end(&ompi_pubsub_base_components_available)) {
        opal_list_item_t* next = opal_list_get_next(item);
        ompi_pubsub_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (ompi_pubsub_base_component_t *) cli->cli_component;
        if(component != best_component) {
            
            OPAL_OUTPUT_VERBOSE((10, ompi_pubsub_base_output,
                                 "ompi:pubsub:base:select: module %s unloaded",
                                 component->pubsub_version.mca_component_name));
            
            mca_base_component_repository_release((mca_base_component_t *) component);
            opal_list_remove_item(&ompi_pubsub_base_components_available, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }
    
    /* init the selected module */
    if (NULL != ompi_pubsub.init) {
        if (OMPI_SUCCESS != (rc = ompi_pubsub.init())) {
            return rc;
        }
    }
    return OMPI_SUCCESS;
}
