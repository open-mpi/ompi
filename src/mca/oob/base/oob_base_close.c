/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/base/base.h"


int mca_oob_base_close(void)
{
  /* Close all remaining available modules (may be one if this is a
     LAM RTE program, or [possibly] multiple if this is laminfo) */

  mca_base_modules_close(mca_oob_base_output, 
                         &mca_oob_base_modules_available, NULL);

  /* All done */

  return LAM_SUCCESS;
}
