/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_call_errhandler = PMPI_Comm_call_errhandler
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif
    

int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == comm ||
        MPI_COMM_NULL == comm) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Comm_call_errhandler");
    }
  }

  /* Invoke the errhandler */

  return LAM_ERRHANDLER_INVOKE(comm, errorcode,
                               "MPI_Comm_call_errhandler");
}
