/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


int mca_oob_base_close(void)
{
  ompi_list_item_t* item;
  /* Finalize all the ptl components and free their list items */
                                                                                                                   
  for (item = ompi_list_remove_first(&mca_oob_base_components);
       NULL != item;
       item = ompi_list_remove_first(&mca_oob_base_components)) {
    mca_base_component_list_item_t* component = (mca_base_component_list_item_t *) item;
                                                                                                                   
    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore) */
                                                                                                                   
    ((mca_oob_base_component_t*)component->cli_component)->oob_finalize();
    free(component);
  }

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_oob_base_output, &mca_oob_base_components, 
                            NULL);

  OBJ_DESTRUCT(&mca_oob_base_modules);
  OBJ_DESTRUCT(&mca_oob_base_components);

  /* All done */

  return OMPI_SUCCESS;
}
