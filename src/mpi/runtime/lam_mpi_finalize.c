/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "mpi.h"
#include "mpi/runtime/runtime.h"

int lam_mpi_finalize(void)
{
  int ret;

  lam_mpi_finalized = 1;
  if (LAM_SUCCESS != (ret = lam_finalize())) {
    return ret;
  }

  /* All done */

  return MPI_SUCCESS;
}
