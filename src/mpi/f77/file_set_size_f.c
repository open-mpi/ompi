/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_SET_SIZE = mpi_file_set_size_f
#pragma weak pmpi_file_set_size = mpi_file_set_size_f
#pragma weak pmpi_file_set_size_ = mpi_file_set_size_f
#pragma weak pmpi_file_set_size__ = mpi_file_set_size_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_SET_SIZE,
                           pmpi_file_set_size,
                           pmpi_file_set_size_,
                           pmpi_file_set_size__,
                           pmpi_file_set_size_f,
                           (MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr),
                           (fh, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_SIZE = mpi_file_set_size_f
#pragma weak mpi_file_set_size = mpi_file_set_size_f
#pragma weak mpi_file_set_size_ = mpi_file_set_size_f
#pragma weak mpi_file_set_size__ = mpi_file_set_size_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_SET_SIZE,
                           mpi_file_set_size,
                           mpi_file_set_size_,
                           mpi_file_set_size__,
                           mpi_file_set_size_f,
                           (MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr),
                           (fh, size, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_set_size_f(MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr)
{

}
