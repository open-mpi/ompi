/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_SIZE = mpi_group_size_f
#pragma weak pmpi_group_size = mpi_group_size_f
#pragma weak pmpi_group_size_ = mpi_group_size_f
#pragma weak pmpi_group_size__ = mpi_group_size_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_SIZE,
                           pmpi_group_size,
                           pmpi_group_size_,
                           pmpi_group_size__,
                           pmpi_group_size_f,
                           (MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr),
                           (group, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_SIZE = mpi_group_size_f
#pragma weak mpi_group_size = mpi_group_size_f
#pragma weak mpi_group_size_ = mpi_group_size_f
#pragma weak mpi_group_size__ = mpi_group_size_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GROUP_SIZE,
                           mpi_group_size,
                           mpi_group_size_,
                           mpi_group_size__,
                           mpi_group_size_f,
                           (MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr),
                           (group, size, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_group_size_f(MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr)
{

}
