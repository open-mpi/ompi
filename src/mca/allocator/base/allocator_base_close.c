/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/allocator/base/base.h"


int mca_allocator_base_close(void)
{
  ompi_list_item_t *item;
  mca_allocator_base_selected_module_t *sm;

  /* Finalize all the allocator modules and free their list items */

  for (item = ompi_list_remove_first(&mca_allocator_base_modules_initialized);
       NULL != item; 
       item = ompi_list_remove_first(&mca_allocator_base_modules_initialized)) {
    sm = (mca_allocator_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This module is going away, so errors don't matter
       anymore) */

    sm->absm_actions->alc_finalize(sm->absm_actions);
    free(sm);
  }

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_modules_close(mca_allocator_base_output, 
                         &mca_allocator_base_modules_available, NULL);

  /* All done */

  return OMPI_SUCCESS;
}

