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
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "include/constants.h"

int mca_base_components_close(int output_id, 
                              ompi_list_t *components_available, 
                              const mca_base_component_t *skip)
{
  ompi_list_item_t *item;
  mca_base_component_priority_list_item_t *pcli, *skipped_pcli = NULL;
  const mca_base_component_t *component;

  /* Close and unload all components in the available list, except the
     "skip" item.  This is handy to close out all non-selected
     components.  It's easier to simply remove the entire list and
     then simply re-add the skip entry when done. */

  for (item = ompi_list_remove_first(components_available);
       NULL != item; 
       item = ompi_list_remove_first(components_available)) {
    pcli = (mca_base_component_priority_list_item_t *) item;
    component = pcli->super.cli_component;

    if (component != skip) {

      /* Close */


      if (NULL != component->mca_close_component) {
        component->mca_close_component();
        ompi_output_verbose(10, output_id, 
                            "mca: base: close: component %s closed",
                           component->mca_component_name);
      }

      /* Unload */

      ompi_output_verbose(10, output_id, 
                          "mca: base: close: unloading component %s",
                         component->mca_component_name);
      mca_base_component_repository_release((mca_base_component_t *) component);
      free(pcli);
    } else {
      skipped_pcli = pcli;
    }
  }

  /* If we found it, re-add the skipped component to the available
     list (see above comment) */

  if (NULL != skipped_pcli) {
    ompi_list_append(components_available, (ompi_list_item_t *) skipped_pcli);
  }

  /* All done */

  return OMPI_SUCCESS;
}
