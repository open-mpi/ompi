/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_FREE = mpi_comm_free_f
#pragma weak pmpi_comm_free = mpi_comm_free_f
#pragma weak pmpi_comm_free_ = mpi_comm_free_f
#pragma weak pmpi_comm_free__ = mpi_comm_free_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_FREE,
                           pmpi_comm_free,
                           pmpi_comm_free_,
                           pmpi_comm_free__,
                           pmpi_comm_free_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_FREE = mpi_comm_free_f
#pragma weak mpi_comm_free = mpi_comm_free_f
#pragma weak mpi_comm_free_ = mpi_comm_free_f
#pragma weak mpi_comm_free__ = mpi_comm_free_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_FREE,
                           mpi_comm_free,
                           mpi_comm_free_,
                           mpi_comm_free__,
                           mpi_comm_free_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

void mpi_comm_free_f(MPI_Fint *comm, MPI_Fint *ierr)
{

}
