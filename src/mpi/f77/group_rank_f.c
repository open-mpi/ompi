/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_RANK = mpi_group_rank_f
#pragma weak pmpi_group_rank = mpi_group_rank_f
#pragma weak pmpi_group_rank_ = mpi_group_rank_f
#pragma weak pmpi_group_rank__ = mpi_group_rank_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_RANK,
                           pmpi_group_rank,
                           pmpi_group_rank_,
                           pmpi_group_rank__,
                           pmpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_RANK = mpi_group_rank_f
#pragma weak mpi_group_rank = mpi_group_rank_f
#pragma weak mpi_group_rank_ = mpi_group_rank_f
#pragma weak mpi_group_rank__ = mpi_group_rank_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_RANK,
                           mpi_group_rank,
                           mpi_group_rank_,
                           mpi_group_rank__,
                           mpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif

void mpi_group_rank_f(MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr)
{

}
