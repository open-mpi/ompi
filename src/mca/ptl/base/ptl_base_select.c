/*
 * $HEADER$
 */

#include "lam_config.h"

#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"


/**
 * Function for weeding out ptl modules that don't want to run.
 *
 * Call the init function on all available modules to find out if they
 * want to run.  Select all modules that don't fail.  Failing modules
 * will be closed and unloaded.  The selected modules will be returned
 * to the caller in a lam_list_t.
 */
int mca_ptl_base_select(bool *allow_multi_user_threads, 
                        bool *have_hidden_threads)
{
  int i, num_ptls;
  bool user_threads, hidden_threads;
  lam_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_ptl_base_module_t *module;
  mca_ptl_t **actions;
  mca_ptl_base_selected_module_t *sm;

  /* Traverse the list of available modules; call their init
     functions. */

  for (item = lam_list_get_first(&mca_ptl_base_modules_available);
       lam_list_get_end(&mca_ptl_base_modules_available) != item;
       item = lam_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_ptl_base_module_t *) mli->mli_module;

    lam_output_verbose(10, mca_ptl_base_output, 
                       "select: initializing %s module %s",
                       module->ptlm_version.mca_type_name,
                       module->ptlm_version.mca_module_name);
    if (NULL == module->ptlm_init) {
      lam_output_verbose(10, mca_ptl_base_output,
                         "select: no init function; ignoring module");
    } else {
      actions = module->ptlm_init(&num_ptls, &user_threads,
                                  &hidden_threads);

      /* If the module didn't initialize, unload it */

      if (NULL == actions) {
        lam_output_verbose(10, mca_ptl_base_output,
                           "select: init returned failure");

        mca_base_module_repository_release((mca_base_module_t *) module);
        lam_output_verbose(10, mca_ptl_base_output,
                           "select: module %s unloaded",
                           module->ptlm_version.mca_module_name);
      } 

      /* Otherwise, it initialized properly.  Save it. */

      else {
        *allow_multi_user_threads |= user_threads;
        *have_hidden_threads |= hidden_threads;

        lam_output_verbose(10, mca_ptl_base_output,
                           "select: init returned success");

        for (i = 0; i < num_ptls; ++i) {
          sm = malloc(sizeof(mca_ptl_base_selected_module_t));
          if (NULL == sm) {
            return LAM_ERR_OUT_OF_RESOURCE;
          }
          OBJ_CONSTRUCT(sm, lam_list_item_t);
          sm->pbsm_module = module;
          sm->pbsm_actions = actions[i];
          lam_list_append(&mca_ptl_base_modules_initialized,
                          (lam_list_item_t*) sm);
        }
        free(actions);
      }
    }
  }

  /* Finished querying all modules.  Check for the bozo case. */

  if (0 == lam_list_get_size(&mca_ptl_base_modules_initialized)) {
    /* JMS Replace with show_help */
    lam_abort(1, "No ptl module available.  This shouldn't happen.");
  }

  /* All done */

  return LAM_SUCCESS;
}
