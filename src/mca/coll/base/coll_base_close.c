/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


int mca_coll_base_close(void)
{
  extern ompi_list_t mca_coll_base_modules_opened;

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_modules_close(mca_coll_base_output, 
                         &mca_coll_base_modules_opened, NULL);

  /* All done */

  return OMPI_SUCCESS;
}
