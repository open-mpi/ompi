/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/oob/base/base.h"

#include "mca/soh/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/soh/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
int mca_soh_base_output = -1;
mca_soh_base_module_t ompi_soh_monitor = {
    mca_soh_base_update_cell_soh_not_available
};
bool mca_soh_base_selected = false;
ompi_list_t mca_soh_base_components_available;
mca_soh_base_component_t mca_soh_base_selected_component;



/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_soh_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("soh", 0, mca_soh_base_static_components, 
                               &mca_soh_base_components_available)) {
    return OMPI_ERROR;
  }

  /* setup output for debug messages */
  if (!ompi_output_init) {  /* can't open output */
      return OMPI_ERROR;
  }

  mca_soh_base_output = ompi_output_open(NULL);

  /* All done */

  return OMPI_SUCCESS;
}
