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

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"


int mca_mpool_base_close(void)
{
  ompi_list_item_t *item;
  mca_mpool_base_selected_module_t *sm;

  /* Finalize all the mpool components and free their list items */

  for (item = ompi_list_remove_first(&mca_mpool_base_components);
       NULL != item; 
       item = ompi_list_remove_first(&mca_mpool_base_components)) {
    sm = (mca_mpool_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore) */

    sm->mpool_module->mpool_finalize(sm->mpool_module);
    free(sm);
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_mpool_base_output, 
                            &mca_mpool_base_components, NULL);

  /* All done */

  return OMPI_SUCCESS;
}
