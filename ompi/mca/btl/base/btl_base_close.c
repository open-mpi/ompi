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
#include "event/event.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"


int mca_btl_base_close(void)
{
  opal_list_item_t *item;
  mca_btl_base_selected_module_t *sm;

  /* disable event processing while cleaning up btls */
  ompi_event_disable();

  /* Finalize all the btl components and free their list items */

  for (item = opal_list_remove_first(&mca_btl_base_modules_initialized);
       NULL != item; 
       item = opal_list_remove_first(&mca_btl_base_modules_initialized)) {
    sm = (mca_btl_base_selected_module_t *) item;

    /* Blatebtly ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore) */

    sm->btl_module->btl_finalize(sm->btl_module);
    free(sm);
  }

  /* Close all remaining opened components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */
  
  if (0 != opal_list_get_size(&mca_btl_base_components_opened)) {
      mca_base_components_close(mca_btl_base_output, 
                                &mca_btl_base_components_opened, NULL);
  }

  /* cleanup */
  if(NULL != mca_btl_base_include)
     free(mca_btl_base_include);
  if(NULL != mca_btl_base_exclude)
     free(mca_btl_base_exclude);

  /* restore event processing */
  ompi_event_enable();

  /* All done */
  return OMPI_SUCCESS;
}
