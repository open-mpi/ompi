/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ALLGATHER = mpi_allgather_f
#pragma weak pmpi_allgather = mpi_allgather_f
#pragma weak pmpi_allgather_ = mpi_allgather_f
#pragma weak pmpi_allgather__ = mpi_allgather_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ALLGATHER,
                           pmpi_allgather,
                           pmpi_allgather_,
                           pmpi_allgather__,
                           pmpi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLGATHER = mpi_allgather_f
#pragma weak mpi_allgather = mpi_allgather_f
#pragma weak mpi_allgather_ = mpi_allgather_f
#pragma weak mpi_allgather__ = mpi_allgather_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ALLGATHER,
                           mpi_allgather,
                           mpi_allgather_,
                           mpi_allgather__,
                           mpi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif

void mpi_allgather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr)
{

}
