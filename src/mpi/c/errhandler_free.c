/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_free = PMPI_Errhandler_free
#endif


int MPI_Errhandler_free(MPI_Errhandler *errhandler)
{
  return MPI_SUCCESS;
}
