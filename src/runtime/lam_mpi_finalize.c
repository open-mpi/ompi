/*
 * $HEADER$
 */

#include "lam_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mpi.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "op/op.h"
#include "runtime/runtime.h"
#include "mca/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


int lam_mpi_finalize(void)
{
  int ret;

  lam_mpi_finalized = true;

  /* Close down MCA modules */

  if (LAM_SUCCESS != (ret = mca_coll_base_close())) {
    return ret;
  }
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

  /* Close down the MCA */

  if (LAM_SUCCESS != (ret = mca_base_close())) {
    return ret;
  }

  /* Leave LAM */

  /* free op resources */
  if (LAM_SUCCESS != (ret = lam_op_finalize())) {
      return ret;
  }

  /* free ddt resources */
  if (LAM_SUCCESS != (ret = lam_ddt_finalize())) {
      return ret;
  }

  /* free communicator resources */
  if (LAM_SUCCESS != (ret = lam_comm_finalize())) {
      return ret;
  }

  /* free group resources */
  if (LAM_SUCCESS != (ret = lam_group_finalize())) {
      return ret;
  }

  /* free errhandler resources */
  if (LAM_SUCCESS != (ret = lam_errhandler_finalize())) {
      return ret;
  }

  if (LAM_SUCCESS != (ret = lam_finalize())) {
    return ret;
  }

  /* All done */

  return MPI_SUCCESS;
}
