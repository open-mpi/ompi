/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_DISCONNECT = mpi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect = mpi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect_ = mpi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect__ = mpi_comm_disconnect_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_DISCONNECT,
                           pmpi_comm_disconnect,
                           pmpi_comm_disconnect_,
                           pmpi_comm_disconnect__,
                           pmpi_comm_disconnect_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_DISCONNECT = mpi_comm_disconnect_f
#pragma weak mpi_comm_disconnect = mpi_comm_disconnect_f
#pragma weak mpi_comm_disconnect_ = mpi_comm_disconnect_f
#pragma weak mpi_comm_disconnect__ = mpi_comm_disconnect_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_DISCONNECT,
                           mpi_comm_disconnect,
                           mpi_comm_disconnect_,
                           mpi_comm_disconnect__,
                           mpi_comm_disconnect_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_disconnect_f(MPI_Fint *comm, MPI_Fint *ierr)
{

}
