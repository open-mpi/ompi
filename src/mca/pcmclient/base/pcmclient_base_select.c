/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"

/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules and get their
 * priorities.  Select the module with the highest priority.  All
 * other modules will be closed and unloaded.
 */
int mca_pcmclient_base_select(bool *allow_multi_user_threads, 
                        bool *have_hidden_threads)
{
  int priority, best_priority;
  bool user_threads, hidden_threads;
  bool best_user_threads, best_hidden_threads;
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  mca_pcmclient_base_component_t *component, *best_component;
  mca_pcmclient_base_module_t *module, *best_module;
  extern ompi_list_t mca_pcmclient_base_components_available;

  ompi_output_verbose(10, mca_pcmclient_base_output,
                      "pcmclient: base: select: started selection code");

  /* Traverse the list of available components; call their init
     functions. */

  best_priority = -1;
  best_component = NULL;
  for (item = ompi_list_get_first(&mca_pcmclient_base_components_available);
       ompi_list_get_end(&mca_pcmclient_base_components_available) != item;
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (mca_pcmclient_base_component_t *) cli->cli_component;

    ompi_output_verbose(10, mca_pcmclient_base_output, 
                       "pcmclient: base: select: initializing %s component %s",
                       component->pcmclient_version.mca_type_name,
                       component->pcmclient_version.mca_component_name);
    if (NULL == component->pcmclient_init) {
      ompi_output_verbose(10, mca_pcmclient_base_output,
                         "pcmclient: base: select: no init function; "
                          "ignoring component");
    } else {
      module = component->pcmclient_init(&priority, &user_threads, &hidden_threads);
      if (NULL == module) {
        ompi_output_verbose(10, mca_pcmclient_base_output,
                           "pcmclient: base: select: init returned failure");
      } else {
        ompi_output_verbose(10, mca_pcmclient_base_output,
                           "pcmclient: base: select: init returned priority %d",
                            priority);
        if (priority > best_priority) {
          best_priority = priority;
          best_user_threads = user_threads;
          best_hidden_threads = hidden_threads;
          best_component = component;
          best_module = module;
        }
      }
    }
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (NULL == best_component) {
    /* JMS Replace with show_help */
    ompi_abort(1, "No pcmclient component available.  This shouldn't happen.");
  } 

  /* Finalize all non-selected components */

  for (item = ompi_list_get_first(&mca_pcmclient_base_components_available);
       ompi_list_get_end(&mca_pcmclient_base_components_available) != item;
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (mca_pcmclient_base_component_t *) cli->cli_component;

    if (component != best_component) {

      /* Finalize */

      if (NULL != component->pcmclient_finalize) {

        /* Blatently ignore the return code (what would we do to
           recover, anyway?  This component is going away, so errors
           don't matter anymore) */

        component->pcmclient_finalize();
        ompi_output_verbose(10, mca_pcmclient_base_output, 
                           "pcmclient: base: select: component %s finalized",
                           component->pcmclient_version.mca_component_name);
      }
    }
  }

  /* This base function closes, unloads, and removes from the
     available list all unselected components.  The available list will
     contain only the selected component. */

  mca_base_components_close(mca_pcmclient_base_output,
                            &mca_pcmclient_base_components_available, 
                            (mca_base_component_t *) best_component);

  /* Save the winner */

  mca_pcmclient_base_selected_component = *best_component;
  mca_pcmclient = *best_module;
  *allow_multi_user_threads = best_user_threads;
  *have_hidden_threads = best_hidden_threads;
  ompi_output_verbose(5, mca_pcmclient_base_output, 
                     "pcmclient: base: select: component %s initialized",
                     mca_pcmclient_base_selected_component.pcmclient_version.mca_component_name);

  ompi_output_verbose(10, mca_pcmclient_base_output,
                      "pcmclient: base: select: completed");

  /* All done */

  return OMPI_SUCCESS;
}
