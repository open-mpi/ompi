/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_GROUP = mpi_comm_group_f
#pragma weak pmpi_comm_group = mpi_comm_group_f
#pragma weak pmpi_comm_group_ = mpi_comm_group_f
#pragma weak pmpi_comm_group__ = mpi_comm_group_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GROUP,
                           pmpi_comm_group,
                           pmpi_comm_group_,
                           pmpi_comm_group__,
                           pmpi_comm_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GROUP = mpi_comm_group_f
#pragma weak mpi_comm_group = mpi_comm_group_f
#pragma weak mpi_comm_group_ = mpi_comm_group_f
#pragma weak mpi_comm_group__ = mpi_comm_group_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GROUP,
                           mpi_comm_group,
                           mpi_comm_group_,
                           mpi_comm_group__,
                           mpi_comm_group_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                           (comm, group, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_group_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr)
{
    MPI_Group c_group;
    MPI_Comm c_comm = MPI_Comm_f2c( *comm );
    
    *ierr = OMPI_INT_2_FINT(MPI_Comm_group( c_comm, &c_group));
    *group = MPI_Group_c2f (c_group);
}
