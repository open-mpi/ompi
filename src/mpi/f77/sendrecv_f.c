/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_SENDRECV = mpi_sendrecv_f
#pragma weak pmpi_sendrecv = mpi_sendrecv_f
#pragma weak pmpi_sendrecv_ = mpi_sendrecv_f
#pragma weak pmpi_sendrecv__ = mpi_sendrecv_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_SENDRECV,
                           pmpi_sendrecv,
                           pmpi_sendrecv_,
                           pmpi_sendrecv__,
                           pmpi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV = mpi_sendrecv_f
#pragma weak mpi_sendrecv = mpi_sendrecv_f
#pragma weak mpi_sendrecv_ = mpi_sendrecv_f
#pragma weak mpi_sendrecv__ = mpi_sendrecv_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_SENDRECV,
                           mpi_sendrecv,
                           mpi_sendrecv_,
                           mpi_sendrecv__,
                           mpi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#endif

void mpi_sendrecv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{

}
