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
#include "mca/errmgr/base/base.h"


int mca_errmgr_base_close(void)
{
  /* If we have a selected component and module, then finalize it */

  if (mca_errmgr_base_selected) {
      mca_errmgr_base_selected_component.errmgr_finalize();
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is mca_info) */

  mca_base_components_close(mca_errmgr_base_output, 
                            &mca_errmgr_base_components_available, NULL);

  /* All done */

  return OMPI_SUCCESS;
}
