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
#pragma weak MPI_Op_create = PMPI_Op_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Op_create(MPI_User_function *function, int commute,
                  MPI_Op *op) 
{
  int err = MPI_SUCCESS;

  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == function || 
        NULL == op) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Comm_create_op");
    }
  }

  /* Create and cache the op.  Sets a refcount of 1. */

  *op = ompi_op_create((bool) commute, (ompi_op_fortran_handler_fn_t*) function);
  if (NULL == *op) {
    err = MPI_ERR_INTERN;
  }

  OMPI_ERRHANDLER_RETURN(err, MPI_COMM_WORLD, MPI_ERR_INTERN,
                        "MPI_Comm_create_op");
}
