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
#pragma weak MPI_Comm_set_errhandler = PMPI_Comm_set_errhandler
#endif


int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == comm || 
        MPI_COMM_NULL == comm) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Comm_set_errhandler");
    } else if (NULL == errhandler ||
               MPI_ERRHANDLER_NULL == errhandler ||
               LAM_ERRHANDLER_TYPE_COMM != errhandler->eh_mpi_object_type) {
      return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                               "MPI_Comm_set_errhandler");
    }
  }

  /* We have a valid comm and errhandler */

  comm->error_handler = errhandler;

  /* All done */
  
  return MPI_SUCCESS;
}
