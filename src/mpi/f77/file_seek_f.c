/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_SEEK = mpi_file_seek_f
#pragma weak pmpi_file_seek = mpi_file_seek_f
#pragma weak pmpi_file_seek_ = mpi_file_seek_f
#pragma weak pmpi_file_seek__ = mpi_file_seek_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_SEEK,
                           pmpi_file_seek,
                           pmpi_file_seek_,
                           pmpi_file_seek__,
                           pmpi_file_seek_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SEEK = mpi_file_seek_f
#pragma weak mpi_file_seek = mpi_file_seek_f
#pragma weak mpi_file_seek_ = mpi_file_seek_f
#pragma weak mpi_file_seek__ = mpi_file_seek_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_SEEK,
                           mpi_file_seek,
                           mpi_file_seek_,
                           mpi_file_seek__,
                           mpi_file_seek_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_seek_f(MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *whence, MPI_Fint *ierr)
{

}
