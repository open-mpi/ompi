/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_INTERSECTION = mpi_group_intersection_f
#pragma weak pmpi_group_intersection = mpi_group_intersection_f
#pragma weak pmpi_group_intersection_ = mpi_group_intersection_f
#pragma weak pmpi_group_intersection__ = mpi_group_intersection_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_INTERSECTION,
                           pmpi_group_intersection,
                           pmpi_group_intersection_,
                           pmpi_group_intersection__,
                           pmpi_group_intersection_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_INTERSECTION = mpi_group_intersection_f
#pragma weak mpi_group_intersection = mpi_group_intersection_f
#pragma weak mpi_group_intersection_ = mpi_group_intersection_f
#pragma weak mpi_group_intersection__ = mpi_group_intersection_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_INTERSECTION,
                           mpi_group_intersection,
                           mpi_group_intersection_,
                           mpi_group_intersection__,
                           mpi_group_intersection_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_group_intersection_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{

}
