/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Init = MPI_Init
#endif


int
MPI_Init(int *argc, char ***argv)
{
  return MPI_SUCCESS;
}
