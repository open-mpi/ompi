/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_RANK = mpi_comm_rank_f
#pragma weak pmpi_comm_rank = mpi_comm_rank_f
#pragma weak pmpi_comm_rank_ = mpi_comm_rank_f
#pragma weak pmpi_comm_rank__ = mpi_comm_rank_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_RANK,
                           pmpi_comm_rank,
                           pmpi_comm_rank_,
                           pmpi_comm_rank__,
                           pmpi_comm_rank_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, rank, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_RANK = mpi_comm_rank_f
#pragma weak mpi_comm_rank = mpi_comm_rank_f
#pragma weak mpi_comm_rank_ = mpi_comm_rank_f
#pragma weak mpi_comm_rank__ = mpi_comm_rank_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_RANK,
                           mpi_comm_rank,
                           mpi_comm_rank_,
                           mpi_comm_rank__,
                           mpi_comm_rank_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, rank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_rank_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr)
{

}
