/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_get = PMPI_Errhandler_get
#endif


int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (MPI_COMM_NULL == comm) {
      return LAM_ERRHDL_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                               "MPI_Errhandler_get");
    } else if (NULL == errhandler) {
      return LAM_ERRHDL_INVOKE(comm, MPI_ERR_ARG, "MPI_Errhandler_get");
    }
  }

  /* This is the backwards compatability function for communicator
     errorhandlers */

  *errhandler = comm->error_handler;

  /* All done */

  return MPI_SUCCESS;
}
