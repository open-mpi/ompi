/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_RANGE_EXCL = mpi_group_range_excl_f
#pragma weak pmpi_group_range_excl = mpi_group_range_excl_f
#pragma weak pmpi_group_range_excl_ = mpi_group_range_excl_f
#pragma weak pmpi_group_range_excl__ = mpi_group_range_excl_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_RANGE_EXCL,
                           pmpi_group_range_excl,
                           pmpi_group_range_excl_,
                           pmpi_group_range_excl__,
                           pmpi_group_range_excl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranges3, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranges3, newgroup, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_RANGE_EXCL = mpi_group_range_excl_f
#pragma weak mpi_group_range_excl = mpi_group_range_excl_f
#pragma weak mpi_group_range_excl_ = mpi_group_range_excl_f
#pragma weak mpi_group_range_excl__ = mpi_group_range_excl_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_RANGE_EXCL,
                           mpi_group_range_excl,
                           mpi_group_range_excl_,
                           mpi_group_range_excl__,
                           mpi_group_range_excl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranges3, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranges3, newgroup, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_group_range_excl_f(MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranges3, MPI_Fint *newgroup, MPI_Fint *ierr)
{

}
