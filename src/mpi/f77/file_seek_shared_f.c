/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_SEEK_SHARED = mpi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared = mpi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared_ = mpi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared__ = mpi_file_seek_shared_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_SEEK_SHARED,
                           pmpi_file_seek_shared,
                           pmpi_file_seek_shared_,
                           pmpi_file_seek_shared__,
                           pmpi_file_seek_shared_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SEEK_SHARED = mpi_file_seek_shared_f
#pragma weak mpi_file_seek_shared = mpi_file_seek_shared_f
#pragma weak mpi_file_seek_shared_ = mpi_file_seek_shared_f
#pragma weak mpi_file_seek_shared__ = mpi_file_seek_shared_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_SEEK_SHARED,
                           mpi_file_seek_shared,
                           mpi_file_seek_shared_,
                           mpi_file_seek_shared__,
                           mpi_file_seek_shared_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_seek_shared_f(MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr)
{

}
