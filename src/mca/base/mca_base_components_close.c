/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"


int mca_base_components_close(int output_id, 
                              ompi_list_t *components_available, 
                              const mca_base_component_t *skip)
{
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  const mca_base_component_t *component;

  /* Close and unload all components in the available list, except the
     "skip" item.  This is handy to close out all non-selected
     components.  It's easier to simply remove the entire list and
     then simply re-add the skip entry when done. */

  for (item = ompi_list_remove_first(components_available);
       NULL != item; 
       item = ompi_list_remove_first(components_available)) {
    cli = (mca_base_component_list_item_t *) item;
    component = cli->cli_component;

    if (component != skip) {

      /* Close */


      if (NULL != component->mca_close_component) {
        component->mca_close_component();
        ompi_output_verbose(10, output_id, "close: component %s closed",
                           component->mca_component_name);
      }

      /* Unload */

      mca_base_component_repository_release((mca_base_component_t *) component);
      ompi_output_verbose(10, output_id, "close: component %s unloaded",
                         component->mca_component_name);
    }
    free(cli);
  }

  /* Re-add the skipped component to the available list (see above
     comment) */

  if (NULL != skip) {
    cli = malloc(sizeof(mca_base_component_list_item_t));
    if (NULL == cli) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    cli->cli_component = skip;
    ompi_list_append(components_available, (ompi_list_item_t *) cli);
  }

  /* All done */

  return OMPI_SUCCESS;
}
