/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lam/runtime/runtime.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/lam/pcm/pcm.h"


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules and get their
 * priorities.  Select the module with the highest priority.  All
 * other modules will be closed and unloaded.
 */
int mca_pcm_base_select(void)
{
  int priority, best_priority;
  lam_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_pcm_base_module_t *module, *best_module;
  mca_pcm_t *actions;
  extern lam_list_t mca_pcm_base_modules_available;

  /* Traverse the list of available modules; call their init
     functions. */

  best_priority = -1;
  best_module = NULL;
  for (item = lam_list_get_first(&mca_pcm_base_modules_available);
       lam_list_get_end(&mca_pcm_base_modules_available) != item;
       item = lam_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_pcm_base_module_t *) mli->mli_module;

    lam_output_verbose(10, mca_pcm_base_output, 
                       "select: initializing %s module %s",
                       module->pcmm_version.mca_type_name,
                       module->pcmm_version.mca_module_name);
    if (NULL == module->pcmm_init) {
      lam_output_verbose(10, mca_pcm_base_output,
                         "select: no init function; ignoring module");
    } else {
      if (MCA_SUCCESS != module->pcmm_init(&priority)) {
        lam_output_verbose(10, mca_pcm_base_output,
                           "select: init returned failure");
      } else {
        lam_output_verbose(10, mca_pcm_base_output,
                           "select: init returned priority %d", priority);
        if (priority > best_priority) {
          best_priority = priority;
          best_module = module;
        }
      }
    }
  }

  /* Finished querying all modules.  Check for the bozo case. */

  if (NULL == best_module) {
    /* JMS Replace with show_help */
    lam_abort(1, "No PCM module available.  This shouldn't happen.");
  } 

  /* Finalize all non-selected modules */

  for (item = lam_list_get_first(&mca_pcm_base_modules_available);
       lam_list_get_end(&mca_pcm_base_modules_available) != item;
       item = lam_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_pcm_base_module_t *) mli->mli_module;

    if (module != best_module) {

      /* Finalize */

      if (NULL != module->pcmm_finalize) {

        /* Blatently ignore the return code (what would we do to
           recover, anyway?  This module is going away, so errors
           don't matter anymore) */

        module->pcmm_finalize();
        lam_output_verbose(10, mca_pcm_base_output, 
                           "select: module %s finalized",
                           module->pcmm_version.mca_module_name);
      }
    }
  }

  /* This base function closes, unloads, and removes from the
     available list all unselected modules.  The available list will
     contain only the selected module. */

  mca_base_modules_close(mca_pcm_base_output, &mca_pcm_base_modules_available, 
                         (mca_base_module_t *) best_module);

  /* Save the winner */

  mca_pcm_base_selected_module = *best_module;
  mca_pcm = *actions;
  lam_output_verbose(10, mca_pcm_base_output, 
                     "select: module %s initialized",
                     module->pcmm_version.mca_module_name);

  /* All done */

  return LAM_SUCCESS;
}
