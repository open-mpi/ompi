/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/base/base.h"


int mca_oob_base_close(void)
{
  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  OBJ_DESTRUCT(&mca_oob_base_modules);
  mca_base_modules_close(mca_oob_base_output, &mca_oob_base_components, NULL);
  /* All done */

  return OMPI_SUCCESS;
}
