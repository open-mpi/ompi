/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FREE_MEM = mpi_free_mem_f
#pragma weak mpi_free_mem = mpi_free_mem_f
#pragma weak mpi_free_mem_ = mpi_free_mem_f
#pragma weak mpi_free_mem__ = mpi_free_mem_f
#if LAM_WANT_MPI_PROFILING
#pragma weak PMPI_FREE_MEM = mpi_free_mem_f
#pragma weak pmpi_free_mem = mpi_free_mem_f
#pragma weak pmpi_free_mem_ = mpi_free_mem_f
#pragma weak pmpi_free_mem__ = mpi_free_mem_f
#endif
#endif


void mpi_free_mem_(char *baseptr, int *ierr)
{
#if 0
  /* JMS Need fortran -> C handle lookups, and a safe way to do the
     baseptr pointer */
  *ierr = MPI_Free_mem(BUF(baseptr));
#endif
}
