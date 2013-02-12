/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/memheap/buddy/memheap_buddy.h"
#include "opal/class/opal_list.h" /*TODO: included in spml/base/base.h remove this include */
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/runtime/opal_progress.h"
/* TODO: remove included in spml.h #include "opal/mca/mca.h" */
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/proc/proc.h"  

typedef struct opened_component_t {
  opal_list_item_t super;
  mca_spml_base_component_t *om_component;
} opened_component_t;

/* TODO: Restore modex.
 static bool modex_reqd=false;
*/


/**
 * Function for selecting one component from all those that are
 * available.
 *
 * Call the init function on all available components and get their
 * priorities.  Select the component with the highest priority.  All
 * other components will be closed and unloaded.  The selected component
 * will have all of its function pointers saved and returned to the
 * caller.
 */
int mca_spml_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int i, priority = 0, best_priority = 0, num_spml = 0;
    int round = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_spml_base_component_t *component = NULL, *best_component = NULL;
    mca_spml_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;
    bool found_spml;
/* TODO:Irit Consider restoring FT
#if OPAL_ENABLE_FT == 1
    mca_spml_base_component_t *wrapper_component = NULL;
    mca_spml_base_module_t *wrapper_module = NULL;
    int wrapper_priority = -1;
#endif
Irit */

    /* Traverse the list of available components; call their init
       functions. */

    best_priority = -1;
    best_component = NULL;
    module = NULL;
    OBJ_CONSTRUCT(&opened, opal_list_t);
    for (round = 0; (round < 2) && (NULL == best_component); round++) {
        for (item = opal_list_get_first(&mca_spml_base_components_available);
             ((opal_list_get_end(&mca_spml_base_components_available) != item) && (item != NULL));
             item = opal_list_get_next(item) ) {
            cli = (mca_base_component_list_item_t *) item;
            component = (mca_spml_base_component_t *) cli->cli_component;

            /* if there is an include list - item must be in the list to be included */
            if (0 == round) {
                found_spml = false;
                for( i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) { 
                    char * tmp_val = NULL;
                    tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml, i);
                    if( NULL == tmp_val) {
                        continue;
                    }
                    if(0 == strncmp(component->spmlm_version.mca_component_name, 
                                    tmp_val, strlen(component->spmlm_version.mca_component_name)) ) {
                        found_spml = true;
                        break;
                    }
                }
            }
            else {
                found_spml = true;
            }

            if(!found_spml && opal_pointer_array_get_size(&mca_spml_base_spml)) { 
                SPML_VERBOSE( 10,"select: component %s not in the include list",
                                         component->spmlm_version.mca_component_name );
                
                continue;
            }

            /* if there is no init function - ignore it */
            if (NULL == component->spmlm_init) {
                SPML_VERBOSE( 10,"select: no init function; ignoring component %s",
                                     component->spmlm_version.mca_component_name );
                continue;
            }

            /* this is a spml that could be considered */
            num_spml++;

            /* Init component to get its priority */
            SPML_VERBOSE( 10,"select: initializing %s component %s",
                                 component->spmlm_version.mca_type_name,
                                 component->spmlm_version.mca_component_name );
            priority = best_priority;
            module = component->spmlm_init(&priority, enable_progress_threads,
                                          enable_mpi_threads);
            if (NULL == module) {
                SPML_VERBOSE( 10,"select: init returned failure for component %s",
                                     component->spmlm_version.mca_component_name );
                continue;
            }

            SPML_VERBOSE( 10,"select: init returned priority %d", priority );


    /* TODO: Consider restoring FT */
    /* Irit
    #if OPAL_ENABLE_FT == 1 */
            /* Determine if this is the wrapper component */
    /*        if( priority <= SPML_SELECT_WRAPPER_PRIORITY) {
                SPML_VERBOSE( 10,"spml:select: Wrapper Component: Component %s was determined to be a Wrapper SPML with priority %d",
                                     component->pmlm_version.mca_component_name, priority );
                wrapper_priority  = priority;
                wrapper_component = component;
                wrapper_module    = module;
                continue;
            }*/
            /* Otherwise determine if this is the best component */
    /*        else 
    #endif
    Irit */

            if (priority > best_priority) {
                best_priority = priority;
                best_component = component;
                best_module = module;
            }
        
            om = (opened_component_t*)malloc(sizeof(opened_component_t));
            if (NULL == om) {
                return OSHMEM_ERR_OUT_OF_RESOURCE;
            }
            OBJ_CONSTRUCT(om, opal_list_item_t);
            om->om_component = component;
            opal_list_append(&opened, (opal_list_item_t*) om);
        }

        /* Sasha: don't think that we need this code, but it can still be useful for debugging */
        if ((0 == round) && (NULL == best_component)) {
            num_spml = 0;
            for( i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) { 
                char * tmp_val = NULL;
                tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml, i);
                if( NULL == tmp_val) {
                    continue;
                }
                SPML_VERBOSE(1, "SPML %s cannot be selected", tmp_val);
            }
        }
    }

    /* Finished querying all components.  Check for the bozo case. */
    
    if( NULL == best_component ) {
        orte_show_help("help-shmem-mca.txt", "find-available:none-found", true, "spml");
        for( i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) { 
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml, i);
            if( NULL == tmp_val) {
                continue;
            }
            orte_errmgr.abort(1, "SPML %s cannot be selected", tmp_val);
        }
        if(0 == i) { 
            orte_errmgr.abort(2, "No spml component available.  This shouldn't happen.");
        }
    } 
    
    SPML_VERBOSE( 10,"selected %s best priority %d\n", 
                         best_component->spmlm_version.mca_component_name, best_priority);

    
/* TODO: restore. Irit removed temporarily */
    /* if more than one SPML could be considered, then we still need the
     * modex since we cannot know which one will be selected on all procs
     */
/*    if (1 < num_spml) {
        modex_reqd = true;
    }
*/

    
    /* Finalize all non-selected components */
/*TODO: IRIT suggestion why not replace with mca_base_components_close with best_component and wrapper (in case of FT) at skip? */ 
    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;

        if (om->om_component != best_component
/* TODO: Consider restoring FT */
/* Irit
#if OPAL_ENABLE_FT == 1
            && om->om_component != wrapper_component
#endif
Irit */
            ) {
            /* Finalize */
                
            if (NULL != om->om_component->spmlm_finalize) {
                
                /* Blatently ignore the return code (what would we do to
                   recover, anyway?  This component is going away, so errors
                   don't matter anymore) */
                
                om->om_component->spmlm_finalize();
                SPML_VERBOSE(10, "select: component %s not selected / finalized",
                                    om->om_component->spmlm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om );
        free(om);
    }
    OBJ_DESTRUCT( &opened );
/*TODO: Irit end of suggestion */

/* TODO: Consider restoring FT*/
#if 0
#if OPAL_ENABLE_FT == 1
    /* Remove the wrapper component from the mca_spml_base_components_available list
     * so we don't unload it prematurely in the next call
     */
    if( NULL != wrapper_component ) {
        for (item  = opal_list_get_first(&mca_spml_base_components_available);
             item != opal_list_get_end(&mca_spml_base_components_available);
             item  = opal_list_get_next(item) ) {
            cli       = (mca_base_component_list_item_t *) item;
            component = (mca_spml_base_component_t *) cli->cli_component;
            
            if( component == wrapper_component ) {
                opal_list_remove_item(&mca_spml_base_components_available, item);
            }
        }
    }
#endif
#endif

    /* Save the winner */

    mca_spml_base_selected_component = *best_component;
    mca_spml = *best_module;
    SPML_VERBOSE( 10, "select: component %s selected",
                         mca_spml_base_selected_component.spmlm_version.mca_component_name );

    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */

    mca_base_components_close(mca_spml_base_output, 
                              &mca_spml_base_components_available, 
                              (mca_base_component_t *) best_component);
    
/* TODO: Consider restoring FT */
#if 0
#if OPAL_ENABLE_FT == 1
    /* If we have a wrapper then initalize it */
    if( NULL != wrapper_component ) {
        priority = SPML_SELECT_WRAPPER_PRIORITY;
        SPML_VERBOSE( 10,"spml:select: Wrapping: Component %s [%d] is being wrapped by component %s [%d]", 
                             mca_spml_base_selected_component.spmlm_version.mca_component_name,
                             best_priority,
                             wrapper_component->spmlm_version.mca_component_name,
                             wrapper_priority );

        /* Ask the wrapper commponent to wrap around the currently
         * selected component. Indicated by the priority value provided
         * this will cause the wrapper to do something different this time around
         */
        module = wrapper_component->spmlm_init(&priority,
                                              enable_progress_threads,
                                              enable_mpi_threads);
        /* Replace with the wrapper */
        best_component = wrapper_component;
        mca_spml_base_selected_component = *best_component;
        best_module = module;
        mca_spml     = *best_module;
    }
#endif
#endif
    /* register the winner's callback */
    /*if( NULL != mca_spml.spml_progress ) {
        opal_progress_register(mca_spml.spml_progress);
    }*/


/* TODO: Restore. Irit Disabled temporarily. */
    /* register winner in the modex */
/*    if (modex_reqd && 0 == ORTE_PROC_MY_NAME->vpid) {
        mca_spml_base_spml_selected(best_component->spmlm_version.mca_component_name);
    }
*/

    /* All done */

    return OSHMEM_SUCCESS;
}
