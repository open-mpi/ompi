/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/registry/registry.h"
#include "mca/registry/base/base.h"


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules and get their
 * priorities.  Select the module with the highest priority.  All
 * other modules will be closed and unloaded.
 */
int mca_registry_base_select(bool *allow_multi_user_threads, 
                        bool *have_hidden_threads)
{
  int priority, best_priority;
  bool user_threads, hidden_threads;
  bool best_user_threads, best_hidden_threads;
  ompi_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_registry_base_module_t *module, *best_module;
  mca_registry_t *actions, *best_actions;
  extern ompi_list_t mca_registry_base_modules_available;

  /* Traverse the list of available modules; call their init
     functions. */

  best_priority = -1;
  best_module = NULL;
  for (item = ompi_list_get_first(&mca_registry_base_modules_available);
       ompi_list_get_end(&mca_registry_base_modules_available) != item;
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_registry_base_module_t *) mli->mli_module;

    ompi_output_verbose(10, mca_registry_base_output, 
                       "select: initializing %s module %s",
                       module->registrym_version.mca_type_name,
                       module->registrym_version.mca_module_name);
    if (NULL == module->registrym_init) {
      ompi_output_verbose(10, mca_registry_base_output,
                         "select: no init function; ignoring module");
    } else {
      actions = module->registrym_init(&priority, &user_threads, 
                                       &hidden_threads);
      if (NULL == actions) {
        ompi_output_verbose(10, mca_registry_base_output,
                           "select: init returned failure");
      } else {
        ompi_output_verbose(10, mca_registry_base_output,
                           "select: init returned priority %d", priority);
        if (priority > best_priority) {
          best_priority = priority;
          best_user_threads = user_threads;
          best_hidden_threads = hidden_threads;
          best_module = module;
          best_actions = actions;
        }
      }
    }
  }

  /* Finished querying all modules.  Check for the bozo case. */

  if (NULL == best_module) {
    /* JMS Replace with show_help */
    ompi_abort(1, "No REGISTRY module available.  This shouldn't happen.");
  } 

  /* Finalize all non-selected modules */

  for (item = ompi_list_get_first(&mca_registry_base_modules_available);
       ompi_list_get_end(&mca_registry_base_modules_available) != item;
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_registry_base_module_t *) mli->mli_module;

    if (module != best_module) {

      /* Finalize */

      if (NULL != module->registrym_finalize) {

        /* Blatently ignore the return code (what would we do to
           recover, anyway?  This module is going away, so errors
           don't matter anymore) */

        module->registrym_finalize();
        ompi_output_verbose(10, mca_registry_base_output, 
                           "select: module %s finalized",
                           module->registrym_version.mca_module_name);
      }
    }
  }

  /* This base function closes, unloads, and removes from the
     available list all unselected modules.  The available list will
     contain only the selected module. */

  mca_base_modules_close(mca_registry_base_output,
                         &mca_registry_base_modules_available, 
                         (mca_base_module_t *) best_module);

  /* Save the winner */

  mca_registry_base_selected_module = *best_module;
  mca_registry = *best_actions;
  *allow_multi_user_threads = best_user_threads;
  *have_hidden_threads = best_hidden_threads;
  ompi_output_verbose(10, mca_registry_base_output, 
                     "select: module %s initialized",
                     module->registrym_version.mca_module_name);

  /* All done */

  return OMPI_SUCCESS;
}
