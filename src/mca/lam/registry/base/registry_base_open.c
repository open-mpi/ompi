/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/lam/registry/registry.h"
#include "mca/lam/registry/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/lam/registry/base/static-modules.h"


/*
 * Global variables
 */
int mca_registry_base_output = -1;
mca_registry_t mca_registry;
lam_list_t mca_registry_base_modules_available;
mca_registry_base_module_t mca_registry_base_selected_module;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_registry_base_open(void)
{
  /* Open up all available modules */

  if (LAM_SUCCESS != 
      mca_base_modules_open("registry", 0, mca_registry_base_static_modules, 
                            &mca_registry_base_modules_available)) {
    return LAM_ERROR;
  }

  /* All done */

  return LAM_SUCCESS;
}
