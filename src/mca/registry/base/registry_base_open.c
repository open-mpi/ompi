/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/registry/registry.h"
#include "mca/registry/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/registry/base/static-modules.h"


/*
 * Global variables
 */
int mca_registry_base_output = -1;
mca_registry_t mca_registry;
ompi_list_t mca_registry_base_modules_available;
mca_registry_base_module_t mca_registry_base_selected_module;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_registry_base_open(void)
{
  /* Open up all available modules */

  if (OMPI_SUCCESS != 
      mca_base_modules_open("registry", 0, mca_registry_base_static_modules, 
                            &mca_registry_base_modules_available)) {
    return OMPI_ERROR;
  }

  /* All done */

  return OMPI_SUCCESS;
}
