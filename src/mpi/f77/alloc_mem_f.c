/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem__ = mpi_alloc_mem_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ALLOC_MEM,
                     pmpi_alloc_mem,
                     pmpi_alloc_mem_,
                     pmpi_alloc_mem__,
                     pmpi_alloc_mem_f,
                     (MPI_Fint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr), 
                     (size, info, baseptr, ierr))
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem__ = mpi_alloc_mem_f
#endif

#if ! LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
LAM_GENERATE_F77_BINDINGS (MPI_ALLOC_MEM,
                     mpi_alloc_mem,
                     mpi_alloc_mem_,
                     mpi_alloc_mem__,
                     mpi_alloc_mem_f,
                     (MPI_Fint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr), 
                     (size, info, baseptr, ierr))
#endif

void mpi_alloc_mem_f(MPI_Fint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr)
{
#if 0
  /* JMS Need fortran -> C handle lookups, and a safe way to do the
     baseptr pointer */
  *ierr = MPI_Alloc_mem(*size, GETHDL(*info), BUF(baseptr));
#endif
}
