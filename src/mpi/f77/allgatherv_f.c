/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ALLGATHERV = mpi_allgatherv_f
#pragma weak pmpi_allgatherv = mpi_allgatherv_f
#pragma weak pmpi_allgatherv_ = mpi_allgatherv_f
#pragma weak pmpi_allgatherv__ = mpi_allgatherv_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ALLGATHERV,
                           pmpi_allgatherv,
                           pmpi_allgatherv_,
                           pmpi_allgatherv__,
                           pmpi_allgatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLGATHERV = mpi_allgatherv_f
#pragma weak mpi_allgatherv = mpi_allgatherv_f
#pragma weak mpi_allgatherv_ = mpi_allgatherv_f
#pragma weak mpi_allgatherv__ = mpi_allgatherv_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ALLGATHERV,
                           mpi_allgatherv,
                           mpi_allgatherv_,
                           mpi_allgatherv__,
                           mpi_allgatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierr) )
#endif

void mpi_allgatherv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr)
{

}
