/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_DIFFERENCE = mpi_group_difference_f
#pragma weak pmpi_group_difference = mpi_group_difference_f
#pragma weak pmpi_group_difference_ = mpi_group_difference_f
#pragma weak pmpi_group_difference__ = mpi_group_difference_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_DIFFERENCE,
                           pmpi_group_difference,
                           pmpi_group_difference_,
                           pmpi_group_difference__,
                           pmpi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_DIFFERENCE = mpi_group_difference_f
#pragma weak mpi_group_difference = mpi_group_difference_f
#pragma weak mpi_group_difference_ = mpi_group_difference_f
#pragma weak mpi_group_difference__ = mpi_group_difference_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_DIFFERENCE,
                           mpi_group_difference,
                           mpi_group_difference_,
                           mpi_group_difference__,
                           mpi_group_difference_f,
                           (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group1, group2, newgroup, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_group_difference_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *newgroup, MPI_Fint *ierr)
{

}
