/*
 * $HEADER$
 */

#include "ompi_config.h"

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


int ompi_mpi_finalize(void)
{
  int ret;

  ompi_mpi_finalized = true;

  /* Close down MCA modules */

  if (OMPI_SUCCESS != (ret = mca_coll_base_close())) {
    return ret;
  }
  if (OMPI_SUCCESS != (ret = mca_ptl_base_close())) {
    return ret;
  }
  if (OMPI_SUCCESS != (ret = mca_pml_base_close())) {
    return ret;
  }

  /* Leave the RTE */

  if (OMPI_SUCCESS != (ret = ompi_rte_finalize())) {
    return ret;
  }

  /* Close down the MCA */

  if (OMPI_SUCCESS != (ret = mca_base_close())) {
    return ret;
  }

  /* Leave OMPI */

  /* free op resources */
  if (OMPI_SUCCESS != (ret = ompi_op_finalize())) {
      return ret;
  }

  /* free ddt resources */
  if (OMPI_SUCCESS != (ret = ompi_ddt_finalize())) {
      return ret;
  }

  /* free communicator resources */
  if (OMPI_SUCCESS != (ret = ompi_comm_finalize())) {
      return ret;
  }

  /* free group resources */
  if (OMPI_SUCCESS != (ret = ompi_group_finalize())) {
      return ret;
  }

  /* free errhandler resources */
  if (OMPI_SUCCESS != (ret = ompi_errhandler_finalize())) {
      return ret;
  }

  if (OMPI_SUCCESS != (ret = ompi_finalize())) {
    return ret;
  }

  /* All done */

  return MPI_SUCCESS;
}
