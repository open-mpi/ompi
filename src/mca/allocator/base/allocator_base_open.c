/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/allocator/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/allocator/base/static-modules.h"


/*
 * Global variables
 */
int mca_allocator_base_output = -1;
ompi_list_t mca_allocator_base_modules_available;
ompi_list_t mca_allocator_base_modules_initialized;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_allocator_base_open(void)
{
  /* Open up all available modules */

  if (OMPI_SUCCESS != 
      mca_base_modules_open("allocator", 0, mca_allocator_base_static_modules, 
                            &mca_allocator_base_modules_available)) {
    return OMPI_ERROR;
  }

  /* Initialize the list so that in mca_allocator_base_close(), we can
     iterate over it (even if it's empty, as in the case of
     ompi_info) */

  OBJ_CONSTRUCT(&mca_allocator_base_modules_initialized, ompi_list_t);

  /* All done */

  return OMPI_SUCCESS;
}

