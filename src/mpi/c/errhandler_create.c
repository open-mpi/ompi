/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_create = PMPI_Errhandler_create
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Errhandler_create(MPI_Handler_function *function,
                          MPI_Errhandler *errhandler)
{
  /* This is a deprecated -- just turn around and call the real
     function */

  return MPI_Comm_create_errhandler(function, errhandler);
}
