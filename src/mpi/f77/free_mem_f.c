/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FREE_MEM = mpi_free_mem_f
#pragma weak pmpi_free_mem = mpi_free_mem_f
#pragma weak pmpi_free_mem_ = mpi_free_mem_f
#pragma weak pmpi_free_mem__ = mpi_free_mem_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FREE_MEM,
                           pmpi_free_mem,
                           pmpi_free_mem_,
                           pmpi_free_mem__,
                           pmpi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FREE_MEM = mpi_free_mem_f
#pragma weak mpi_free_mem = mpi_free_mem_f
#pragma weak mpi_free_mem_ = mpi_free_mem_f
#pragma weak mpi_free_mem__ = mpi_free_mem_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FREE_MEM,
                           mpi_free_mem,
                           mpi_free_mem_,
                           mpi_free_mem__,
                           mpi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_free_mem_f(char *base, MPI_Fint *ierr)
{

}
