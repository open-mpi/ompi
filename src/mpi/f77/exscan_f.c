/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_EXSCAN = mpi_exscan_f
#pragma weak pmpi_exscan = mpi_exscan_f
#pragma weak pmpi_exscan_ = mpi_exscan_f
#pragma weak pmpi_exscan__ = mpi_exscan_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_EXSCAN,
                           pmpi_exscan,
                           pmpi_exscan_,
                           pmpi_exscan__,
                           pmpi_exscan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_EXSCAN = mpi_exscan_f
#pragma weak mpi_exscan = mpi_exscan_f
#pragma weak mpi_exscan_ = mpi_exscan_f
#pragma weak mpi_exscan__ = mpi_exscan_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_EXSCAN,
                           mpi_exscan,
                           mpi_exscan_,
                           mpi_exscan__,
                           mpi_exscan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_exscan_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr)
{

}
