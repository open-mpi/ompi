/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"


/**
 * Container for list of components opened
 *
 * \internal
 */
typedef struct opened_component_t {
  /** make us a list item */
  ompi_list_item_t super;
  /** component that has been opened */
  mca_llm_base_component_t *oc_component;
} opened_component_t;


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
                    mca_llm_base_module_t *selected, 
                    bool have_threads)
{
  int priority, best_priority;
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  mca_llm_base_component_t *component, *best_component;
  mca_llm_base_module_t *module, *best_module;
  ompi_list_t opened;
  opened_component_t *oc;  

  ompi_output_verbose(10, mca_llm_base_output,
                      "llm: base: select: started selection code");

  /* Traverse the list of available components; call their init
     functions. */

  best_priority = -1;
  best_component = NULL;
  OBJ_CONSTRUCT(&opened, ompi_list_t);
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
          best_priority = priority;
          best_component = component;
          best_module = module;
        }

        oc = malloc(sizeof(opened_component_t));
        if (NULL == oc) {
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(oc, ompi_list_item_t);
        oc->oc_component = component;
        ompi_list_append(&opened, (ompi_list_item_t*) oc);
      }
    }
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (NULL == best_component) {
    /* JMS Replace with show_help */
    ompi_abort(1, "No llm component available.  This shouldn't happen.");
  } 

  /* Finalize all non-selected components */

  for (item = ompi_list_remove_first(&opened);
       NULL != item;
       item = ompi_list_remove_first(&opened)) {
    oc = (opened_component_t *) item;
    if (oc->oc_component != best_component) {

      /* Finalize */

      if (NULL != oc->oc_component->llm_finalize) {

        /* Blatently ignore the return code (what would we do to
           recover, anyway?  This component is going away, so errors
           don't matter anymore) */

        oc->oc_component->llm_finalize();
        ompi_output_verbose(10, mca_llm_base_output, 
                           "llm: base: select: "
                            "component %s not selected / finalized",
                           component->llm_version.mca_component_name);
      }
    }
    free(oc);
  }

  /* This base function closes, unloads, and removes from the
     available list all unselected components.  The available list will
     contain only the selected component. */

  mca_base_components_close(mca_llm_base_output, 
                         &mca_llm_base_components_available, 
                         (mca_base_component_t *) best_component);

  /* Save the winner */

  mca_llm_base_selected_component = *best_component;
  *selected = *best_module;
  ompi_output_verbose(5, mca_llm_base_output, 
                     "llm: base: select: component %s selected",
                     component->llm_version.mca_component_name);

  OBJ_DESTRUCT(&opened);

  ompi_output_verbose(10, mca_llm_base_output,
                      "llm: base: select: completed");

  /* All done */

  return OMPI_SUCCESS;
}

