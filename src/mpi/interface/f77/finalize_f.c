/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZE = mpi_finalize_f
#pragma weak mpi_finalize = mpi_finalize_f
#pragma weak mpi_finalize_ = mpi_finalize_f
#pragma weak mpi_finalize__ = mpi_finalize_f
#if LAM_WANT_MPI_PROFILING
#pragma weak PMPI_FINALIZE = mpi_finalize_f
#pragma weak pmpi_finalize = mpi_finalize_f
#pragma weak pmpi_finalize_ = mpi_finalize_f
#pragma weak pmpi_finalize__ = mpi_finalize_f
#endif
#endif


void
mpi_finalize_f(int *ierror)
{
  *ierror = MPI_Finalize();
}
