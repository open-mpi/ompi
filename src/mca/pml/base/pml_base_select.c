/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"


typedef struct opened_component_t {
  ompi_list_item_t super;
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
int mca_pml_base_select(mca_pml_base_module_t *selected,
                        bool *allow_multi_user_threads,
                        bool *have_hidden_threads)
{
  int priority=0, best_priority=0;
  bool user_threads=false, hidden_threads=false;
  bool best_user_threads=false, best_hidden_threads=false;
  ompi_list_item_t *item=NULL;
  mca_base_component_list_item_t *cli=NULL;
  mca_pml_base_component_t *component=NULL, *best_component=NULL;
  mca_pml_base_module_t *modules=NULL;
  ompi_list_t opened;
  opened_component_t *om=NULL;  

  /* Traverse the list of available components; call their init
     functions. */

  best_priority = -1;
  best_component = NULL;
  modules = NULL;
  best_user_threads = user_threads = true;
  best_hidden_threads = hidden_threads = false;
  OBJ_CONSTRUCT(&opened, ompi_list_t);
  for (item = ompi_list_get_first(&mca_pml_base_components_available);
       ompi_list_get_end(&mca_pml_base_components_available) != item;
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (mca_pml_base_component_t *) cli->cli_component;

    ompi_output_verbose(10, mca_pml_base_output, 
                       "select: initializing %s component %s",
                       component->pmlm_version.mca_type_name,
                       component->pmlm_version.mca_component_name);
    if (NULL == component->pmlm_init) {
      ompi_output_verbose(10, mca_pml_base_output,
                         "select: no init function; ignoring component");
    } else {
      modules = component->pmlm_init(&priority, &user_threads,
                                  &hidden_threads);
      if (NULL == modules) {
        ompi_output_verbose(10, mca_pml_base_output,
                           "select: init returned failure");
      } else {
        ompi_output_verbose(10, mca_pml_base_output,
                           "select: init returned priority %d", priority);
        if (priority > best_priority) {
          best_priority = priority;
          best_user_threads = user_threads;
          best_hidden_threads = hidden_threads;
          best_component = component;
        }

        om = malloc(sizeof(opened_component_t));
        if (NULL == om) {
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, ompi_list_item_t);
        om->om_component = component;
        ompi_list_append(&opened, (ompi_list_item_t*) om);
      }
    }
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (NULL == best_component) {
    /* JMS Replace with show_help */
    orte_abort(1, "No pml component available.  This shouldn't happen.");
  } 

  /* Finalize all non-selected components */

  for (item = ompi_list_remove_first(&opened);
       NULL != item;
       item = ompi_list_remove_first(&opened)) {
    om = (opened_component_t *) item;
    if (om->om_component != best_component) {

      /* Finalize */

      if (NULL != om->om_component->pmlm_finalize) {

        /* Blatently ignore the return code (what would we do to
           recover, anyway?  This component is going away, so errors
           don't matter anymore) */

        om->om_component->pmlm_finalize();
        ompi_output_verbose(10, mca_pml_base_output, 
                           "select: component %s not selected / finalized",
                           component->pmlm_version.mca_component_name);
      }
    }
    free(om);
  }

  /* This base function closes, unloads, and removes from the
     available list all unselected components.  The available list will
     contain only the selected component. */

  mca_base_components_close(mca_pml_base_output, &mca_pml_base_components_available, 
                         (mca_base_component_t *) best_component);

  /* Save the winner */

  mca_pml_base_selected_component = *best_component;
  mca_pml = *modules;
  *selected = *modules;
  *allow_multi_user_threads &= best_user_threads;
  *have_hidden_threads |= best_hidden_threads;
  ompi_output_verbose(10, mca_pml_base_output, 
                     "select: component %s selected",
                     component->pmlm_version.mca_component_name);

  /* register the winner's callback */
  ompi_progress_register(mca_pml.pml_progress);

  /* All done */

  return OMPI_SUCCESS;
}
