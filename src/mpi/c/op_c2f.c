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
#pragma weak MPI_Op_c2f = PMPI_Op_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

MPI_Fint MPI_Op_c2f(MPI_Op op) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == op ||
        MPI_OP_NULL == op) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Op_c2f");
    }
  }

  /* All done */

  return (MPI_Fint) op->o_f_to_c_index;
}
