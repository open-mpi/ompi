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
#pragma weak MPI_Comm_get_errhandler = PMPI_Comm_get_errhandler
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == comm || 
        MPI_COMM_NULL == comm) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Comm_get_errhandler");
    } else if (NULL == errhandler) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Comm_get_errhandler");
    }
  }

  /* Return the errhandler.  Do not increase the refcount here; we
     only refcount on communicators */

  *errhandler = comm->error_handler;

  /* All done */

  return MPI_SUCCESS;
}
