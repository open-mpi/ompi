/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "mca/oob/oob.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/oob/base/static-components.h"


/*
 * Global variables
 */
mca_oob_t mca_oob;
int mca_oob_base_output = -1;
char* mca_oob_base_include = NULL;
char* mca_oob_base_exclude = NULL;
ompi_list_t mca_oob_base_components;
ompi_list_t mca_oob_base_modules;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_oob_base_open(void)
{
  /* Open up all available components */

  OBJ_CONSTRUCT(&mca_oob_base_components, ompi_list_t);
  OBJ_CONSTRUCT(&mca_oob_base_modules, ompi_list_t);

  if (OMPI_SUCCESS != 
      mca_base_components_open("oob", 0, mca_oob_base_static_components, 
                               &mca_oob_base_components)) {
    return OMPI_ERROR;
  }

  /* register parameters */
  mca_base_param_lookup_string(
      mca_base_param_register_string("oob","base","include",NULL,NULL), &mca_oob_base_include);
  mca_base_param_lookup_string(
      mca_base_param_register_string("oob","base","exclude",NULL,NULL), &mca_oob_base_exclude);

  /* All done */
  return OMPI_SUCCESS;
}

