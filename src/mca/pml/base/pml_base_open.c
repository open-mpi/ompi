/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/pml/base/static-modules.h"


/*
 * Global variables
 */
int mca_pml_base_output = -1;
mca_pml_t mca_pml;
ompi_list_t mca_pml_base_modules_available;
mca_pml_base_module_t mca_pml_base_selected_module;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pml_base_open(void)
{
  /* Open up all available modules */

  if (OMPI_SUCCESS != 
      mca_base_modules_open("pml", 0, mca_pml_base_static_modules, 
                            &mca_pml_base_modules_available)) {
    return OMPI_ERROR;
  }

  /* Set a sentinel in case we don't select any modules (e.g.,
     ompi_info) */

  mca_pml_base_selected_module.pmlm_finalize = NULL;

  /* All done */

  return OMPI_SUCCESS;
}
