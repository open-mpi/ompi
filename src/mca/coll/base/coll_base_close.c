/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


int mca_coll_base_close(void)
{
  extern lam_list_t mca_coll_base_modules_available;

  /* Close all remaining available modules (may be one if this is a
     LAM RTE program, or [possibly] multiple if this is laminfo) */

  mca_base_modules_close(mca_coll_base_output, 
                         &mca_coll_base_modules_available, NULL);

  /* All done */

  return LAM_SUCCESS;
}
