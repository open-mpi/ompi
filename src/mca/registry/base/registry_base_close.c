/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/lam/registry/registry.h"
#include "mca/lam/registry/base/base.h"


int mca_registry_base_close(void)
{
  /* Close all remaining available modules (may be one if this is a
     LAM RTE program, or [possibly] multiple if this is laminfo) */

  mca_base_modules_close(mca_registry_base_output, 
                         &mca_registry_base_modules_available, NULL);

  /* All done */

  return LAM_SUCCESS;
}
