/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_FREE = mpi_group_free_f
#pragma weak pmpi_group_free = mpi_group_free_f
#pragma weak pmpi_group_free_ = mpi_group_free_f
#pragma weak pmpi_group_free__ = mpi_group_free_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_FREE,
                           pmpi_group_free,
                           pmpi_group_free_,
                           pmpi_group_free__,
                           pmpi_group_free_f,
                           (MPI_Fint *group, MPI_Fint *ierr),
                           (group, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_FREE = mpi_group_free_f
#pragma weak mpi_group_free = mpi_group_free_f
#pragma weak mpi_group_free_ = mpi_group_free_f
#pragma weak mpi_group_free__ = mpi_group_free_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_FREE,
                           mpi_group_free,
                           mpi_group_free_,
                           mpi_group_free__,
                           mpi_group_free_f,
                           (MPI_Fint *group, MPI_Fint *ierr),
                           (group, ierr) )
#endif

void mpi_group_free_f(MPI_Fint *group, MPI_Fint *ierr)
{

}
