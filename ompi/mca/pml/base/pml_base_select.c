/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
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

#include "opal/class/opal_list.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"


typedef struct opened_component_t {
  opal_list_item_t super;
  mca_pml_base_component_t *om_component;
} opened_component_t;

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
int mca_pml_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int priority = 0, best_priority = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_pml_base_component_t *component = NULL, *best_component = NULL;
    mca_pml_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;

    /* Traverse the list of available components; call their init
       functions. */

    best_priority = -1;
    best_component = NULL;
    module = NULL;
    OBJ_CONSTRUCT(&opened, opal_list_t);
    for (item = opal_list_get_first(&mca_pml_base_components_available);
         opal_list_get_end(&mca_pml_base_components_available) != item;
         item = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_pml_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        if ( NULL != mca_pml_base_pml &&
             strcmp(component->pmlm_version.mca_component_name, mca_pml_base_pml) != 0) {
            opal_output_verbose( 10, mca_pml_base_output,
                                 "select: component %s not in the include list",
                                 component->pmlm_version.mca_component_name );
            continue;
        }

        if (NULL == component->pmlm_init) {
            opal_output_verbose( 10, mca_pml_base_output,
                                 "select: no init function; ignoring component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }
        opal_output_verbose( 10, mca_pml_base_output, 
                             "select: initializing %s component %s",
                             component->pmlm_version.mca_type_name,
                             component->pmlm_version.mca_component_name );
        module = component->pmlm_init(&priority, enable_progress_threads,
                                      enable_mpi_threads);
        if (NULL == module) {
            opal_output_verbose( 10, mca_pml_base_output,
                                 "select: init returned failure for component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }
        opal_output_verbose( 10, mca_pml_base_output,
                             "select: init returned priority %d", priority );
        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }
        
        om = malloc(sizeof(opened_component_t));
        if (NULL == om) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, opal_list_item_t);
        om->om_component = component;
        opal_list_append(&opened, (opal_list_item_t*) om);
    }

    /* Finished querying all components.  Check for the bozo case. */

    if( NULL == best_component ) {
        /* JMS Replace with show_help */
        orte_abort(1, "No pml component available.  This shouldn't happen.");
    } 

    /* Finalize all non-selected components */

    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;
        if (om->om_component != best_component) {

            /* Finalize */
            
            if (NULL != om->om_component->pmlm_finalize) {

                /* Blatently ignore the return code (what would we do to
                   recover, anyway?  This component is going away, so errors
                   don't matter anymore) */

                om->om_component->pmlm_finalize();
                opal_output_verbose(10, mca_pml_base_output, 
                                    "select: component %s not selected / finalized",
                                    component->pmlm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om );
        free(om);
    }
    OBJ_DESTRUCT( &opened );
    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */

    mca_base_components_close(mca_pml_base_output, 
                              &mca_pml_base_components_available, 
                              (mca_base_component_t *) best_component);
    
    /* Save the winner */

    mca_pml_base_selected_component = *best_component;
    mca_pml = *best_module;
    opal_output_verbose( 10, mca_pml_base_output, 
                         "select: component %s selected",
                         best_component->pmlm_version.mca_component_name );
    /* register the winner's callback */
    opal_progress_register(mca_pml.pml_progress);

    /* All done */

    return OMPI_SUCCESS;
}
