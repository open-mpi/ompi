/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_POSITION_SHARED = mpi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared = mpi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared_ = mpi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared__ = mpi_file_get_position_shared_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_GET_POSITION_SHARED,
                           pmpi_file_get_position_shared,
                           pmpi_file_get_position_shared_,
                           pmpi_file_get_position_shared__,
                           pmpi_file_get_position_shared_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_POSITION_SHARED = mpi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared = mpi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared_ = mpi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared__ = mpi_file_get_position_shared_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_GET_POSITION_SHARED,
                           mpi_file_get_position_shared,
                           mpi_file_get_position_shared_,
                           mpi_file_get_position_shared__,
                           mpi_file_get_position_shared_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#endif

void mpi_file_get_position_shared_f(MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *ierr)
{

}
