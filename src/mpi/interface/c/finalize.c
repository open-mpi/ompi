/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Finalize = MPI_Finalize
#endif


int
MPI_Finalize(void)
{
  return MPI_SUCCESS;
}
