/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/gpr/base/base.h"


int mca_gpr_base_close(void)
{
  /* If we have a selected component and module, then finalize it */

  if (mca_gpr_base_selected) {
    mca_gpr_base_selected_component.gpr_finalize();
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_modules_close(mca_gpr_base_output, 
                         &mca_gpr_base_components_available, NULL);

  /* All done */

  return OMPI_SUCCESS;
}
