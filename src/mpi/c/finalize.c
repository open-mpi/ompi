/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Finalize = PMPI_Finalize
#endif


int MPI_Finalize(void)
{
  /* Pretty simple */

  return lam_mpi_finalize();
}
