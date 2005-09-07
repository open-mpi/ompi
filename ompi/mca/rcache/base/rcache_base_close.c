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

#include "ompi/include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/rcache/rcache.h"
#include "mca/rcache/base/base.h"


int mca_rcache_base_close(void)
{
  opal_list_item_t *item;
  mca_rcache_base_selected_module_t *sm;

  /* Finalize all the rcache components and free their list items */

  for (item = opal_list_remove_first(&mca_rcache_base_modules);
       NULL != item; 
       item = opal_list_remove_first(&mca_rcache_base_modules)) {
    sm = (mca_rcache_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore).  Note that it's legal for the module to have NULL for
       the finalize function. */

    if (NULL != sm->rcache_module->rcache_finalize) {
        sm->rcache_module->rcache_finalize(sm->rcache_module);
    }
    OBJ_RELEASE(sm);
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_rcache_base_output, 
                            &mca_rcache_base_components, NULL);

  /* All done */

  return OMPI_SUCCESS;
}
