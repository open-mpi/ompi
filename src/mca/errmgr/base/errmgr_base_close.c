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

#include "orte_config.h"

#include <stdio.h>

#include "include/orte_constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/errmgr/base/base.h"


int orte_errmgr_base_close(void)
{
  /* If we have a selected component and module, then finalize it */

  if (orte_errmgr_base_selected) {
      orte_errmgr_base_selected_component.errmgr_finalize();
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(orte_errmgr_base_output, 
                            &orte_errmgr_base_components_available, NULL);

    orte_errmgr_initialized = false;
  /* All done */

  return ORTE_SUCCESS;
}
