/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_REDUCE = mpi_reduce_f
#pragma weak pmpi_reduce = mpi_reduce_f
#pragma weak pmpi_reduce_ = mpi_reduce_f
#pragma weak pmpi_reduce__ = mpi_reduce_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_REDUCE,
                           pmpi_reduce,
                           pmpi_reduce_,
                           pmpi_reduce__,
                           pmpi_reduce_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, root, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REDUCE = mpi_reduce_f
#pragma weak mpi_reduce = mpi_reduce_f
#pragma weak mpi_reduce_ = mpi_reduce_f
#pragma weak mpi_reduce__ = mpi_reduce_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_REDUCE,
                           mpi_reduce,
                           mpi_reduce_,
                           mpi_reduce__,
                           mpi_reduce_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, root, comm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_reduce_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{

}
