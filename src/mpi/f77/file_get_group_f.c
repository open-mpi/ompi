/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_GROUP = mpi_file_get_group_f
#pragma weak pmpi_file_get_group = mpi_file_get_group_f
#pragma weak pmpi_file_get_group_ = mpi_file_get_group_f
#pragma weak pmpi_file_get_group__ = mpi_file_get_group_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_GET_GROUP,
                           pmpi_file_get_group,
                           pmpi_file_get_group_,
                           pmpi_file_get_group__,
                           pmpi_file_get_group_f,
                           (MPI_Fint *fh, MPI_Fint *group, MPI_Fint *ierr),
                           (fh, group, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_GROUP = mpi_file_get_group_f
#pragma weak mpi_file_get_group = mpi_file_get_group_f
#pragma weak mpi_file_get_group_ = mpi_file_get_group_f
#pragma weak mpi_file_get_group__ = mpi_file_get_group_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_GET_GROUP,
                           mpi_file_get_group,
                           mpi_file_get_group_,
                           mpi_file_get_group__,
                           mpi_file_get_group_f,
                           (MPI_Fint *fh, MPI_Fint *group, MPI_Fint *ierr),
                           (fh, group, ierr) )
#endif

void mpi_file_get_group_f(MPI_Fint *fh, MPI_Fint *group, MPI_Fint *ierr)
{

}
