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
 * component's public mca_base_component_t struct.
 */

#include "mca/pml/base/static-components.h"

static int mca_pml_base_progress(void) 
{
    return OMPI_SUCCESS;
}


/*
 * Global variables
 */
int mca_pml_base_output = -1;
mca_pml_base_module_t mca_pml = {
    NULL,
    NULL,
    NULL,
    NULL,
    mca_pml_base_progress,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

ompi_list_t mca_pml_base_components_available;
mca_pml_base_component_t mca_pml_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pml_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("pml", 0, mca_pml_base_static_components, 
                               &mca_pml_base_components_available)) {
    return OMPI_ERROR;
  }

  /* Set a sentinel in case we don't select any components (e.g.,
     ompi_info) */

  mca_pml_base_selected_component.pmlm_finalize = NULL;

  /* All done */

  return OMPI_SUCCESS;
}
