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

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


int mca_coll_base_close(void)
{
  /* Close all components that are still open.  This may be the opened
     list (if we're in ompi_info), or it may be the available list (if
     we're anywhere else). */

  if (mca_coll_base_components_opened_valid) {
    mca_base_components_close(mca_coll_base_output,
                              &mca_coll_base_components_opened, NULL);
    OBJ_DESTRUCT(&mca_coll_base_components_opened);
    mca_coll_base_components_opened_valid = false;
  } else if (mca_coll_base_components_available_valid) {
    mca_base_components_close(mca_coll_base_output,
                              &mca_coll_base_components_available, NULL);
    OBJ_DESTRUCT(&mca_coll_base_components_available);
    mca_coll_base_components_available_valid = false;
  }

  /* All done */

  return OMPI_SUCCESS;
}
