/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/lfc/lam_list.h"
#include "lam/util/output.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"


int mca_base_modules_close(int output_id, lam_list_t *modules_available, 
                           const mca_base_module_t *skip)
{
  lam_list_item_t *item;
  mca_base_module_list_item_t *mli;
  const mca_base_module_t *module;

  /* Close and unload all modules in the available list, except the
     "skip" item.  This is handy to close out all non-selected
     modules.  It's easier to simply remove the entire list and then
     simply re-add the skip entry when done. */

  for (item = lam_list_remove_first(modules_available);
       NULL != item; 
       item = lam_list_remove_first(modules_available)) {
    mli = (mca_base_module_list_item_t *) item;
    module = mli->mli_module;

    if (module != skip) {

      /* Close */


      if (NULL != module->mca_close_module) {
        module->mca_close_module();
        lam_output_verbose(10, output_id, "close: module %s closed",
                           module->mca_module_name);
      }

      /* Unload */

      mca_base_module_registry_release((mca_base_module_t *) module);
      lam_output_verbose(10, output_id, "close: module %s unloaded",
                         module->mca_module_name);
    }
    free(mli);
  }

  /* Re-add the skipped module to the available list (see above
     comment) */

  if (NULL != skip) {
    mli = malloc(sizeof(mca_base_module_list_item_t));
    if (NULL == mli) {
      return LAM_ERR_OUT_OF_RESOURCE;
    }
    mli->mli_module = skip;
    lam_list_append(modules_available, (lam_list_item_t *) mli);
  }

  /* All done */

  return LAM_SUCCESS;
}
