/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "mpi.h"
#include "mpi/runtime/runtime.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/pml/pml.h"


int lam_mpi_finalize(void)
{
  int ret;

  lam_mpi_finalized = true;

  /* Close down MCA modules */

  if (LAM_SUCCESS != (ret = mca_ptl_base_close())) {
    return ret;
  }

  if (LAM_SUCCESS != (ret = mca_pml_base_close())) {
    return ret;
  }

  /* Leave the RTE */

  if (LAM_SUCCESS != (ret = lam_rte_finalize())) {
    return ret;
  }

  /* Leave LAM */

  if (LAM_SUCCESS != (ret = lam_finalize())) {
    return ret;
  }

  /* All done */

  return MPI_SUCCESS;
}
