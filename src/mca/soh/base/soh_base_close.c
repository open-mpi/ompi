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
#include "mca/soh/base/base.h"


int orte_soh_base_close(void)
{
  /* If we have a selected component and module, then finalize it */

  if (NULL != orte_soh.finalize) {
    orte_soh.finalize();
  }

  /* after the module, close the component?? */
  /* orte_soh_base_component_finalize (); */

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(orte_soh_base.soh_output, 
                            &orte_soh_base.soh_components, NULL);

  /* All done */

  return OMPI_SUCCESS;
}

