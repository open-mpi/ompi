/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_FREE = mpi_comm_free_f
#pragma weak pmpi_comm_free = mpi_comm_free_f
#pragma weak pmpi_comm_free_ = mpi_comm_free_f
#pragma weak pmpi_comm_free__ = mpi_comm_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_FREE,
                           pmpi_comm_free,
                           pmpi_comm_free_,
                           pmpi_comm_free__,
                           pmpi_comm_free_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_FREE = mpi_comm_free_f
#pragma weak mpi_comm_free = mpi_comm_free_f
#pragma weak mpi_comm_free_ = mpi_comm_free_f
#pragma weak mpi_comm_free__ = mpi_comm_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_FREE,
                           mpi_comm_free,
                           mpi_comm_free_,
                           mpi_comm_free__,
                           mpi_comm_free_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_free_f(MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Comm_free ( &c_comm );
    *comm = MPI_Comm_c2f (c_comm);
}
