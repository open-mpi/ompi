/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <syslog.h>

#include "mca/base/base.h"
#include "mca/base/base.h"


/*
 * Public variables
 */
int mca_base_mpi_param_check_param = -1;


int mca_base_open(void)
{
  mca_base_mpi_param_check_param = 
    mca_base_param_register_int("base", NULL, "mpi_param_check", 
                                "mpi_param_check", 1);

  /* All done */

  return LAM_SUCCESS;
}
