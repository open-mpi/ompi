/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_SCATTER = mpi_scatter_f
#pragma weak pmpi_scatter = mpi_scatter_f
#pragma weak pmpi_scatter_ = mpi_scatter_f
#pragma weak pmpi_scatter__ = mpi_scatter_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_SCATTER,
                           pmpi_scatter,
                           pmpi_scatter_,
                           pmpi_scatter__,
                           pmpi_scatter_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCATTER = mpi_scatter_f
#pragma weak mpi_scatter = mpi_scatter_f
#pragma weak mpi_scatter_ = mpi_scatter_f
#pragma weak mpi_scatter__ = mpi_scatter_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_SCATTER,
                           mpi_scatter,
                           mpi_scatter_,
                           mpi_scatter__,
                           mpi_scatter_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif

void mpi_scatter_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{

}
