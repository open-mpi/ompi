/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/oob/base/static-modules.h"


/*
 * Global variables
 */
mca_oob_t mca_oob;
int mca_oob_base_output = -1;
ompi_list_t mca_oob_base_components;
ompi_list_t mca_oob_base_modules;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_oob_base_open(void)
{
  /* Open up all available modules */

  if (OMPI_SUCCESS != 
      mca_base_modules_open("oob", 0, mca_oob_base_static_modules, 
                            &mca_oob_base_components)) {
    return OMPI_ERROR;
  }

  /* All done */
  OBJ_CONSTRUCT(&mca_oob_base_modules, ompi_list_t);
  return OMPI_SUCCESS;
}

