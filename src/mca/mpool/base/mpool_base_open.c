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
#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "include/constants.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#ifdef WIN32
    const mca_base_component_t *mca_mpool_base_static_components[] = {NULL};
#else 
#include "mca/mpool/base/static-components.h"
#endif


/*
 * Global variables
 */
int mca_mpool_base_output = -1;
ompi_list_t mca_mpool_base_components;
ompi_list_t mca_mpool_base_modules;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_mpool_base_open(void)
{
  /* Open up all available components - and populate the
     mca_mpool_base_components list */

  if (OMPI_SUCCESS != 
      mca_base_components_open("mpool", 0, mca_mpool_base_static_components, 
                               &mca_mpool_base_components)) {
    return OMPI_ERROR;
  }

  /* Initialize the list so that in mca_mpool_base_close(), we can
     iterate over it (even if it's empty, as in the case of
     ompi_info) */

  OBJ_CONSTRUCT(&mca_mpool_base_modules, ompi_list_t);

  /* All done */

  return OMPI_SUCCESS;
}
