/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem__ = mpi_alloc_mem_f
#if LAM_WANT_MPI_PROFILING
#pragma weak PMPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem__ = mpi_alloc_mem_f
#endif
#endif


void mpi_alloc_mem_(int *size, int *info, char *baseptr, int *ierr)
{
#if 0
  /* JMS Need fortran -> C handle lookups, and a safe way to do the
     baseptr pointer */
  *ierr = MPI_Alloc_mem(*size, GETHDL(*info), BUF(baseptr));
#endif
}
