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
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"


int mca_pml_base_close(void)
{
  /* Blatently ignore the return code (what would we do to recover,
     anyway?  This module is going away, so errors don't matter
     anymore) */

  mca_pml.pml_progress = mca_pml_base_progress;
  if (NULL != mca_pml_base_selected_component.pmlm_finalize) {
    mca_pml_base_selected_component.pmlm_finalize();
  }

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_pml_base_output, 
                            &mca_pml_base_components_available, NULL);

  /* All done */

  return OMPI_SUCCESS;
}

