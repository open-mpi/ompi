/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/allocator/base/base.h"


/**
 * Function for weeding out allocator modules that don't want to run.
 *
 * Call the init function on all available modules to find out if they
 * want to run.  Select all modules that don't fail.  Failing modules
 * will be closed and unloaded.  The selected modules will be returned
 * to the caller in a ompi_list_t.
 */
int mca_allocator_base_select(bool *allow_multi_user_threads)
{
#if 0
  int i, num_allocators;
  ompi_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_allocator_base_module_t *module;
  mca_allocator_t **actions;
  mca_allocator_base_selected_module_t *sm;

  /* Traverse the list of available modules; call their init
     functions. */

  for (item = ompi_list_get_first(&mca_allocator_base_modules_available);
       ompi_list_get_end(&mca_allocator_base_modules_available) != item;
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = (mca_allocator_base_module_t *) mli->mli_module;

    ompi_output_verbose(10, mca_allocator_base_output, 
                       "select: initializing %s module %s",
                       module->allocatorm_version.mca_type_name,
                       module->allocatorm_version.mca_module_name);
    if (NULL == module->allocatorm_init) {
      ompi_output_verbose(10, mca_allocator_base_output,
                         "select: no init function; ignoring module");
    } else {
      actions = module->allocatorm_init(&num_allocators, &user_threads,
                                  &hidden_threads);

      /* If the module didn't initialize, unload it */

      if (NULL == actions) {
        ompi_output_verbose(10, mca_allocator_base_output,
                           "select: init returned failure");

        mca_base_module_repository_release((mca_base_module_t *) module);
        ompi_output_verbose(10, mca_allocator_base_output,
                           "select: module %s unloaded",
                           module->allocatorm_version.mca_module_name);
      } 

      /* Otherwise, it initialized properly.  Save it. */

      else {
        *allow_multi_user_threads |= user_threads;
        *have_hidden_threads |= hidden_threads;

        ompi_output_verbose(10, mca_allocator_base_output,
                           "select: init returned success");

        for (i = 0; i < num_allocators; ++i) {
          sm = malloc(sizeof(mca_allocator_base_selected_module_t));
          if (NULL == sm) {
            return OMPI_ERR_OUT_OF_RESOURCE;
          }
          OBJ_CONSTRUCT(sm, ompi_list_item_t);
          sm->pbsm_module = module;
          sm->pbsm_actions = actions[i];
          ompi_list_append(&mca_allocator_base_modules_initialized,
                          (ompi_list_item_t*) sm);
        }
        free(actions);
      }
    }
  }

  /* Finished querying all modules.  Check for the bozo case. */

  if (0 == ompi_list_get_size(&mca_allocator_base_modules_initialized)) {
    /* JMS Replace with show_help */
    ompi_abort(1, "No allocator module available.  This shouldn't happen.");
  }

  /* All done */

#endif
  return OMPI_SUCCESS;
}

