/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int mca_ns_base_select(bool *allow_multi_user_threads, 
                       bool *have_hidden_threads)
{
  ompi_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_ns_base_component_t *component, *best_component = NULL;
  mca_ns_t *module, *best_module = NULL;
  bool multi, hidden;
  int priority, best_priority = -1;

  /* Iterate through all the available components */

  for (item = ompi_list_get_first(&mca_ns_base_components_available);
       item != ompi_list_get_end(&mca_ns_base_components_available);
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    component = (mca_ns_base_component_t *) mli->mli_module;

    /* Call the component's init function and see if it wants to be
       selected */

    module = component->ns_init(&multi, &hidden, &priority);

    /* If we got a non-NULL module back, then the component wants to
       be selected.  So save its multi/hidden values and save the
       module with the highest priority */

    if (NULL != module) {

      /* If this is the best one, save it */

      if (priority > best_priority) {

        /* If there was a previous best one, finalize */

        if (NULL != best_component) {
          best_component->ns_finalize();
        }

        /* Save the new best one */

        best_module = module;
        best_component = component;
        *allow_multi_user_threads = multi;
        *have_hidden_threads = hidden;
      } 

      /* If it's not the best one, finalize it */

      else {
        component->ns_finalize();
      }
    }
  }

  /* If we didn't find one to select, barf */

  if (NULL == best_component) {
    return OMPI_ERROR;
  }

  /* We have happiness -- save the component and module for later
     usage */

  ompi_name_server = *best_module;
  mca_ns_base_selected_component = *best_component;
  mca_ns_base_selected = true;

  /* all done */

  return OMPI_SUCCESS;
}
