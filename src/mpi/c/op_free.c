/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "op/op.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Op_free = PMPI_Op_free
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Op_free(MPI_Op *op) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == op ||
        ompi_op_is_intrinsic(*op)) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Op_free");
    }
  }

  /* We have a valid op, release it */

  OBJ_RELEASE(*op);
  *op = MPI_OP_NULL;

  /* All done */

  return MPI_SUCCESS;
}
