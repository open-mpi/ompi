/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = mpi_init_f
#pragma weak mpi_init = mpi_init_f
#pragma weak mpi_init_ = mpi_init_f
#pragma weak mpi_init__ = mpi_init_f
#if LAM_WANT_MPI_PROFILING
#pragma weak PMPI_INIT = mpi_init_f
#pragma weak pmpi_init = mpi_init_f
#pragma weak pmpi_init_ = mpi_init_f
#pragma weak pmpi_init__ = mpi_init_f
#endif
#endif


void
mpi_init_f(int *ierror)
{
  *ierror = MPI_Init(NULL, NULL);
}
