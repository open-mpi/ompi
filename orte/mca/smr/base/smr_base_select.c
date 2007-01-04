/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
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
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/smr/base/base.h"
#include "orte/mca/smr/base/smr_private.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_smr_base_select(void)
{
  opal_list_item_t *item;
  opal_list_item_t *best_item = NULL;
  mca_base_component_list_item_t *cli;
  orte_smr_base_component_t *component, *best_component = NULL;
  orte_smr_base_module_t *module, *best_module = NULL;
  int priority, best_priority = -1;

  /* Iterate through all the available components */

  for (item = opal_list_get_first(&orte_smr_base.smr_components);
       item != opal_list_get_end(&orte_smr_base.smr_components);
       item = opal_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = (orte_smr_base_component_t *) cli->cli_component;

    /* Call the component's init function and see if it wants to be
       selected */

    module = component->smr_init(&priority);

    /* If we got a non-NULL module back, then the component wants to
       be selected.  So save its multi/hidden values and save the
       module with the highest priority */

    if (NULL == module) {
        continue;
    }

    /* If this is the best one, save it */

    if (priority > best_priority) {
        /* If there was a previous best one, finalize */
        if (NULL != best_module) {
            best_module->finalize();
            OBJ_RELEASE (best_item);
        }

        /* Save the new best one */
        best_item = item;
        best_module = module;
        best_component = component;

        /* update the best priority */
        best_priority = priority;

        } /* if best by priority */ 

    /* If it's not the best one, finalize it */

     else {
         component->smr_finalize();
     }

  } /* for each possible component */


  /* If we didn't find one to select, barf */
  if (NULL != best_module) {
      orte_smr = *best_module;
  }

  /* all done */

  return ORTE_SUCCESS;
}
