/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "mpi.h"
#include "mpi/runtime/runtime.h"


int lam_mpi_init(int argc, char **argv, int requested, int *provided)
{
  int ret;

  if (LAM_SUCCESS != (ret = lam_init(argc, argv))) {
    return ret;
  }

  if (LAM_SUCCESS != (ret = lam_rte_init())) {
    return ret;
  }

  /* All done */

  lam_mpi_initialized = 1;
  return MPI_SUCCESS;
}
