/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GATHER = mpi_gather_f
#pragma weak pmpi_gather = mpi_gather_f
#pragma weak pmpi_gather_ = mpi_gather_f
#pragma weak pmpi_gather__ = mpi_gather_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GATHER,
                           pmpi_gather,
                           pmpi_gather_,
                           pmpi_gather__,
                           pmpi_gather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GATHER = mpi_gather_f
#pragma weak mpi_gather = mpi_gather_f
#pragma weak mpi_gather_ = mpi_gather_f
#pragma weak mpi_gather__ = mpi_gather_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GATHER,
                           mpi_gather,
                           mpi_gather_,
                           mpi_gather__,
                           mpi_gather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_gather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{

}
