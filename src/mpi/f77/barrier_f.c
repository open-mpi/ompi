/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BARRIER = mpi_barrier_f
#pragma weak pmpi_barrier = mpi_barrier_f
#pragma weak pmpi_barrier_ = mpi_barrier_f
#pragma weak pmpi_barrier__ = mpi_barrier_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BARRIER,
                           pmpi_barrier,
                           pmpi_barrier_,
                           pmpi_barrier__,
                           pmpi_barrier_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BARRIER = mpi_barrier_f
#pragma weak mpi_barrier = mpi_barrier_f
#pragma weak mpi_barrier_ = mpi_barrier_f
#pragma weak mpi_barrier__ = mpi_barrier_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BARRIER,
                           mpi_barrier,
                           mpi_barrier_,
                           mpi_barrier__,
                           mpi_barrier_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_barrier_f(MPI_Fint *comm, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
