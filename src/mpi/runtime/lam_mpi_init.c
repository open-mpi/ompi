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


int lam_mpi_init(int argc, char **argv, int requested, int *provided)
{
  int ret;

  /* Become a LAM process */

  if (LAM_SUCCESS != (ret = lam_init(argc, argv))) {
    return ret;
  }

  /* Join the run-time environment */

  if (LAM_SUCCESS != (ret = lam_rte_init())) {
    return ret;
  }

  /* Open up relevant MCA modules */

  if (LAM_SUCCESS != (ret = mca_pml_base_open())) {
    /* JMS show_help */
    return LAM_ERROR;
  }
  if (LAM_SUCCESS != (ret = mca_ptl_base_open())) {
    /* JMS show_help */
    return LAM_ERROR;
  }

  /* All done */

  lam_mpi_initialized = true;
  return MPI_SUCCESS;
}
