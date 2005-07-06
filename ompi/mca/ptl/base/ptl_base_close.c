/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "opal/event/event.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"


int mca_ptl_base_close(void)
{
  opal_list_item_t *item;
  mca_ptl_base_selected_module_t *sm;

  /* disable event processing while cleaning up ptls */
  opal_event_disable();

  /* Finalize all the ptl components and free their list items */

  for (item = opal_list_remove_first(&mca_ptl_base_modules_initialized);
       NULL != item; 
       item = opal_list_remove_first(&mca_ptl_base_modules_initialized)) {
    sm = (mca_ptl_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore) */

    sm->pbsm_module->ptl_finalize(sm->pbsm_module);
    free(sm);
  }

  /* Close all remaining opened components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  if (0 != opal_list_get_size(&mca_ptl_base_components_opened)) {
    mca_base_components_close(mca_ptl_base_output, 
                              &mca_ptl_base_components_opened, NULL);
  }

  /* cleanup */
  if( NULL != mca_ptl_base_include ) {
     free(mca_ptl_base_include);
     mca_ptl_base_include = NULL;
  }
  if( NULL != mca_ptl_base_exclude ) {
     free(mca_ptl_base_exclude);
     mca_ptl_base_exclude = NULL;
  }

  /* restore event processing */
  opal_event_enable();

  /* All done */
  return OMPI_SUCCESS;
}
