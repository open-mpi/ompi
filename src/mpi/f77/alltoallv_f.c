/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ALLTOALLV = mpi_alltoallv_f
#pragma weak pmpi_alltoallv = mpi_alltoallv_f
#pragma weak pmpi_alltoallv_ = mpi_alltoallv_f
#pragma weak pmpi_alltoallv__ = mpi_alltoallv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALLV,
                           pmpi_alltoallv,
                           pmpi_alltoallv_,
                           pmpi_alltoallv__,
                           pmpi_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALLV = mpi_alltoallv_f
#pragma weak mpi_alltoallv = mpi_alltoallv_f
#pragma weak mpi_alltoallv_ = mpi_alltoallv_f
#pragma weak mpi_alltoallv__ = mpi_alltoallv_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALLV,
                           mpi_alltoallv,
                           mpi_alltoallv_,
                           mpi_alltoallv__,
                           mpi_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_alltoallv_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls,
		     MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts,
		     MPI_Fint *rdispls, MPI_Fint *recvtype, 
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    *ierr = MPI_Alltoallv(sendbuf, sendcounts, sdispls, c_sendtype, 
			  recvbuf, recvcounts, rdispls, c_recvtype, c_comm);

}
