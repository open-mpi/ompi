/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_REMOTE_GROUP = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group_ = mpi_comm_remote_group_f
#pragma weak pmpi_comm_remote_group__ = mpi_comm_remote_group_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_REMOTE_GROUP,
                           pmpi_comm_remote_group,
                           pmpi_comm_remote_group_,
                           pmpi_comm_remote_group__,
                           pmpi_comm_remote_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_REMOTE_GROUP = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group_ = mpi_comm_remote_group_f
#pragma weak mpi_comm_remote_group__ = mpi_comm_remote_group_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_REMOTE_GROUP,
                           mpi_comm_remote_group,
                           mpi_comm_remote_group_,
                           mpi_comm_remote_group__,
                           mpi_comm_remote_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_remote_group_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
