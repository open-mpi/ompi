/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_RECV = mpi_recv_f
#pragma weak pmpi_recv = mpi_recv_f
#pragma weak pmpi_recv_ = mpi_recv_f
#pragma weak pmpi_recv__ = mpi_recv_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_RECV,
                           pmpi_recv,
                           pmpi_recv_,
                           pmpi_recv__,
                           pmpi_recv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RECV = mpi_recv_f
#pragma weak mpi_recv = mpi_recv_f
#pragma weak mpi_recv_ = mpi_recv_f
#pragma weak mpi_recv__ = mpi_recv_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_RECV,
                           mpi_recv,
                           mpi_recv_,
                           mpi_recv__,
                           mpi_recv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, status, ierr) )
#endif

void mpi_recv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{

}
