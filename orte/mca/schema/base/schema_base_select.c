/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "orte/mca/schema/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_schema_base_select(void)
{
  opal_list_item_t *item;
  mca_base_component_list_item_t *cli;
  mca_schema_base_component_t *component, *best_component = NULL;
  orte_schema_base_module_t *module, *best_module = NULL;
  bool multi, hidden;
  int priority, best_priority = -1;

  /* Iterate through all the available components */

  for (item = opal_list_get_first(&orte_schema_base_components_available);
       item != opal_list_get_end(&orte_schema_base_components_available);
       item = opal_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (mca_schema_base_component_t *) cli->cli_component;

    /* Call the component's init function and see if it wants to be
       selected */
    module = component->schema_init(&multi, &hidden, &priority);

    /* If we got a non-NULL module back, then the component wants to
       be selected.  So save its multi/hidden values and save the
       module with the highest priority */

    if (NULL != module) {
      /* If this is the best one, save it */
      if (priority > best_priority) {

        /* If there was a previous best one, finalize */

        if (NULL != best_component) {
          best_component->schema_finalize();
        }

        /* Save the new best one */
        best_module = module;
        best_component = component;

	/* update the best priority */
	best_priority = priority;
      } 

      /* If it's not the best one, finalize it */

      else {
        component->schema_finalize();
      }
    }
  }

  /* If we didn't find one to select, that's okay - stick with default */

  if (NULL == best_component) {
    return ORTE_SUCCESS;
  }

  /* We have happiness -- save the component and module for later
     usage */

  orte_schema = *best_module;
  orte_schema_base_selected_component = *best_component;
  orte_schema_base_selected = true;

  /* all done */

  return ORTE_SUCCESS;
}
