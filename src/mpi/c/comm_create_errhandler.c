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
#pragma weak MPI_Comm_create_errhandler = PMPI_Comm_create_errhandler
#endif


int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *function,
                               MPI_Errhandler *errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == function || 
        NULL == errhandler) {
      return LAM_ERRHDL_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                               "MPI_Errhandler_get");
    }
  }

  /* JMS Continue here */

  /* All done */

  return MPI_SUCCESS;
}
