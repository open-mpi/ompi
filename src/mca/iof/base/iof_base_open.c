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
#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/iof/base/static-components.h"


/*
 * Global variables
 */
int mca_iof_base_output = -1;
ompi_list_t mca_iof_base_components_opened;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_iof_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("iof", 0, mca_iof_base_static_components, 
                               &mca_iof_base_components_opened)) {
    return OMPI_ERROR;
  }

  /* All done */
  return OMPI_SUCCESS;
}

