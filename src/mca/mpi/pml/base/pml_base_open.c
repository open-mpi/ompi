/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/pml/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/mpi/pml/base/static-modules.h"


/*
 * Global variables
 */
int mca_pml_base_output = -1;
mca_pml_t mca_pml;
lam_list_t mca_pml_base_modules_available;
mca_pml_base_module_t mca_pml_base_selected_module;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pml_base_open(void)
{
  /* Open up all available modules */

  if (LAM_SUCCESS != 
      mca_base_modules_open("pml", 0, mca_pml_base_static_modules, 
                            &mca_pml_base_modules_available)) {
    return LAM_ERROR;
  }

  /* All done */

  return LAM_SUCCESS;
}
