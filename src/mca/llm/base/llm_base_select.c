/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "util/show_help.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules and get their
 * priorities.  Select the module with the highest priority.  All
 * other modules will be closed and unloaded.  The selected module
 * will have all of its function pointers saved and returned to the
 * caller.
 */
int
mca_llm_base_select(const char *active_pcm,
                    mca_llm_base_module_t **selected, 
                    bool have_threads)
{
  int priority, best_priority;
  mca_base_component_list_item_t *cli;
  ompi_list_item_t *item;
  mca_llm_base_module_t *module, *best_module;
  mca_llm_base_component_t *component;

  ompi_output_verbose(10, mca_llm_base_output,
                      "llm: base: select: started selection code");

  /* Traverse the list of available components; call their init
     functions. */

  best_priority = -1;
  best_module = NULL;
  for (item = ompi_list_get_first(&mca_llm_base_components_available);
       ompi_list_get_end(&mca_llm_base_components_available) != item;
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (mca_llm_base_component_t *) cli->cli_component;

    ompi_output_verbose(10, mca_llm_base_output, 
                       "llm: base: select: initializing %s component %s",
                       component->llm_version.mca_type_name,
                       component->llm_version.mca_component_name);
    if (NULL == component->llm_init) {
      ompi_output_verbose(10, mca_llm_base_output,
                         "llm: base: select: "
                          "no init function; ignoring component");
    } else {
      module = component->llm_init(active_pcm, have_threads, &priority);
      if (NULL == module) {
        ompi_output_verbose(10, mca_llm_base_output,
                           "llm: base: select: init returned failure");
      } else {
        ompi_output_verbose(10, mca_llm_base_output,
                           "llm: base: select: init returned priority %d", 
                            priority);
        if (priority > best_priority) {
          /* start by killing off the previous guy (loser...) */
          if (NULL != best_module) {
            best_module->llm_finalize((struct mca_llm_base_module_1_0_0_t*) best_module);
          }
          best_priority = priority;
          best_module = module;
        }
      }
    }
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (NULL == best_module) {
      ompi_show_help("help-llm-base.txt", "select:no-module-found", true);
      return OMPI_ERR_NOT_FOUND;
  } 

  /* Save the winner */
  *selected = best_module;

  ompi_output_verbose(10, mca_llm_base_output,
                      "llm: base: select: completed");

  /* All done */
  return OMPI_SUCCESS;
}

