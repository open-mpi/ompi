/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_REMOTE_GROUP = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group_ = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group__ = mpi_comm_remote_group_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_REMOTE_GROUP,
                           pmpi_comm_remote_group,
                           pmpi_comm_remote_group_,
                           pmpi_comm_remote_group__,
                           pmpi_comm_remote_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_REMOTE_GROUP = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group_ = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group__ = mpi_comm_remote_group_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_REMOTE_GROUP,
                           mpi_comm_remote_group,
                           mpi_comm_remote_group_,
                           mpi_comm_remote_group__,
                           mpi_comm_remote_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_remote_group_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr)
{

}
