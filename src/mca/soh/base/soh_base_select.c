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

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"

#include "mca/soh/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_soh_base_select(void)
{
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  orte_soh_base_component_t *component, *best_component = NULL;
  orte_soh_base_module_t *module, *best_module = NULL;
  int priority, best_priority = -1;

  /* Iterate through all the available components */

  for (item = ompi_list_get_first(&orte_soh_base.soh_components);
       item != ompi_list_get_end(&orte_soh_base.soh_components);
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (orte_soh_base_component_t *) cli->cli_component;

    /* Call the component's init function and see if it wants to be
       selected */

    module = component->soh_init(&priority);

    /* If we got a non-NULL module back, then the component wants to
       be selected.  So save its multi/hidden values and save the
       module with the highest priority */

    if (NULL != module) {
		continue;
	}

    /* If this is the best one, save it */

    if (priority > best_priority) {

    	/* If there was a previous best one, finalize */

	   if (NULL != best_module) {
		  best_module->finalize();
	   }

	   /* Save the new best one */

	   best_module = module;
	   best_component = component;

	   /* update the best priority */
	   best_priority = priority;

	} /* if best by priority */ 

    /* If it's not the best one, finalize it */

/*     else { */
/*         component->soh_finalize(); */
/*     }  */

  } /* for each possible component */


  /* If we didn't find one to select, barf */
  if (NULL != best_module) {
      orte_soh = *best_module;
  }

  /* all done */

  return OMPI_SUCCESS;
}
