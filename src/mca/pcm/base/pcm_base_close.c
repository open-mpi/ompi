/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/lam/pcm/pcm.h"


int mca_pcm_base_close(void)
{
  /* Close all remaining available modules (may be one if this is a
     LAM RTE program, or [possibly] multiple if this is laminfo) */

  mca_base_modules_close(mca_pcm_base_output, 
                         &mca_pcm_base_modules_available, NULL);

  /* All done */

  return LAM_SUCCESS;
}
