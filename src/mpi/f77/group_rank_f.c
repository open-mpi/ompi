/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "group/group.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_RANK = mpi_group_rank_f
#pragma weak pmpi_group_rank = mpi_group_rank_f
#pragma weak pmpi_group_rank_ = mpi_group_rank_f
#pragma weak pmpi_group_rank__ = mpi_group_rank_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_RANK,
                           pmpi_group_rank,
                           pmpi_group_rank_,
                           pmpi_group_rank__,
                           pmpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_RANK = mpi_group_rank_f
#pragma weak mpi_group_rank = mpi_group_rank_f
#pragma weak mpi_group_rank_ = mpi_group_rank_f
#pragma weak mpi_group_rank__ = mpi_group_rank_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_RANK,
                           mpi_group_rank,
                           mpi_group_rank_,
                           mpi_group_rank__,
                           mpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_group_rank_f(MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr)
{
  ompi_group_t *c_group;

  /* Make the fortran to c representation conversion */
  c_group = MPI_Group_f2c(*group);
  
  *ierr = MPI_Group_rank(c_group, rank);
}
