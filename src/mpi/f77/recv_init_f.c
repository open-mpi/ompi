/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_RECV_INIT = mpi_recv_init_f
#pragma weak pmpi_recv_init = mpi_recv_init_f
#pragma weak pmpi_recv_init_ = mpi_recv_init_f
#pragma weak pmpi_recv_init__ = mpi_recv_init_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_RECV_INIT,
                           pmpi_recv_init,
                           pmpi_recv_init_,
                           pmpi_recv_init__,
                           pmpi_recv_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RECV_INIT = mpi_recv_init_f
#pragma weak mpi_recv_init = mpi_recv_init_f
#pragma weak mpi_recv_init_ = mpi_recv_init_f
#pragma weak mpi_recv_init__ = mpi_recv_init_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_RECV_INIT,
                           mpi_recv_init,
                           mpi_recv_init_,
                           mpi_recv_init__,
                           mpi_recv_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_recv_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
