/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ALLTOALLW = mpi_alltoallw_f
#pragma weak pmpi_alltoallw = mpi_alltoallw_f
#pragma weak pmpi_alltoallw_ = mpi_alltoallw_f
#pragma weak pmpi_alltoallw__ = mpi_alltoallw_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALLW,
                           pmpi_alltoallw,
                           pmpi_alltoallw_,
                           pmpi_alltoallw__,
                           pmpi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALLW = mpi_alltoallw_f
#pragma weak mpi_alltoallw = mpi_alltoallw_f
#pragma weak mpi_alltoallw_ = mpi_alltoallw_f
#pragma weak mpi_alltoallw__ = mpi_alltoallw_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALLW,
                           mpi_alltoallw,
                           mpi_alltoallw_,
                           mpi_alltoallw__,
                           mpi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_alltoallw_f(char *sendbuf, MPI_Fint *sendcounts,
		     MPI_Fint *sdispls, MPI_Fint *sendtypes, 
		     char *recvbuf, MPI_Fint *recvcounts,
		     MPI_Fint *rdispls, MPI_Fint *recvtypes,
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    *ierr = MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, 
			  recvbuf, recvcounts, rdispls, recvtypes, c_comm);
}
