/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_CREATE = mpi_comm_create_f
#pragma weak pmpi_comm_create = mpi_comm_create_f
#pragma weak pmpi_comm_create_ = mpi_comm_create_f
#pragma weak pmpi_comm_create__ = mpi_comm_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_CREATE,
                           pmpi_comm_create,
                           pmpi_comm_create_,
                           pmpi_comm_create__,
                           pmpi_comm_create_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, group, newcomm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CREATE = mpi_comm_create_f
#pragma weak mpi_comm_create = mpi_comm_create_f
#pragma weak mpi_comm_create_ = mpi_comm_create_f
#pragma weak mpi_comm_create__ = mpi_comm_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_CREATE,
                           mpi_comm_create,
                           mpi_comm_create_,
                           mpi_comm_create__,
                           mpi_comm_create_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, group, newcomm, ierr) )
#endif

void mpi_comm_create_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr)
{

}
