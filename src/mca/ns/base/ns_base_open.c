/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/ns/base/static-modules.h"


/*
 * Global variables
 */
int mca_ns_base_output = -1;
mca_ns_t ompi_name_server;
ompi_list_t mca_ns_base_modules_available;
mca_ns_base_module_t mca_ns_base_selected_module;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_ns_base_open(void)
{
  /* Open up all available modules */

  if (OMPI_SUCCESS != 
      mca_base_modules_open("ns", 0, mca_ns_base_static_modules, 
                            &mca_ns_base_modules_available)) {
    return OMPI_ERROR;
  }

  /* All done */

  return OMPI_SUCCESS;
}
