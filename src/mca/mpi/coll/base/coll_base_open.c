/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "lam/util/cmd_line.h"
#include "mca/mpi/coll/base/base.h"


int mca_coll_base_open(lam_cmd_line_t *cmd)
{
  printf("In mca_coll_base_open\n");
  return LAM_SUCCESS;
}
