/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"


OBJ_CLASS_INSTANCE(mca_mpool_base_selected_module_t, ompi_list_item_t, NULL, NULL);


/**
 * Function for weeding out mpool modules that don't want to run.
 *
 * Call the init function on all available modules to find out if they
 * want to run.  Select all modules that don't fail.  Failing modules
 * will be closed and unloaded.  The selected modules will be returned
 * to the caller in a ompi_list_t.
 */
int mca_mpool_base_init(bool *allow_multi_user_threads)
{
  bool user_threads;
  ompi_list_item_t *item;
  mca_base_module_list_item_t *mli;
  mca_mpool_base_component_t *component;
  mca_mpool_t *module;
  mca_mpool_base_selected_module_t *sm;

  /* Traverse the list of available modules; call their init
     functions. */

  for (item = ompi_list_get_first(&mca_mpool_base_modules_available);
       ompi_list_get_end(&mca_mpool_base_modules_available) != item;
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    component = (mca_mpool_base_component_t *) mli->mli_module;

    ompi_output_verbose(10, mca_mpool_base_output, 
                       "select: initializing %s module %s",
                       component->mpool_version.mca_type_name,
                       component->mpool_version.mca_module_name);
    if (NULL == component->mpool_init) {
      ompi_output_verbose(10, mca_mpool_base_output,
                         "select: no init function; ignoring module");
    } else {
       module = component->mpool_init(&user_threads);

      /* If the module didn't initialize, unload it */

      if (NULL == module) {
        ompi_output_verbose(10, mca_mpool_base_output,
                           "select: init returned failure");

        mca_base_module_repository_release((mca_base_module_t *) component);
        ompi_output_verbose(10, mca_mpool_base_output,
                           "select: component %s unloaded",
                           component->mpool_version.mca_module_name);
      } 

      /* Otherwise, it initialized properly.  Save it. */

      else {
        *allow_multi_user_threads |= user_threads;

        ompi_output_verbose(10, mca_mpool_base_output,
                           "select: init returned success");

          sm = OBJ_NEW(mca_mpool_base_selected_module_t);
          sm->mpool_component = component;
          sm->mpool_module = module;
          ompi_list_append(&mca_mpool_base_modules_initialized, (ompi_list_item_t*) sm);
        }
      }
  }
  return OMPI_SUCCESS;
}

