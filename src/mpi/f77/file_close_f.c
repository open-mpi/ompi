/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_CLOSE = mpi_file_close_f
#pragma weak pmpi_file_close = mpi_file_close_f
#pragma weak pmpi_file_close_ = mpi_file_close_f
#pragma weak pmpi_file_close__ = mpi_file_close_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_CLOSE,
                           pmpi_file_close,
                           pmpi_file_close_,
                           pmpi_file_close__,
                           pmpi_file_close_f,
                           (MPI_Fint *fh, MPI_Fint *ierr),
                           (fh, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CLOSE = mpi_file_close_f
#pragma weak mpi_file_close = mpi_file_close_f
#pragma weak mpi_file_close_ = mpi_file_close_f
#pragma weak mpi_file_close__ = mpi_file_close_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_CLOSE,
                           mpi_file_close,
                           mpi_file_close_,
                           mpi_file_close__,
                           mpi_file_close_f,
                           (MPI_Fint *fh, MPI_Fint *ierr),
                           (fh, ierr) )
#endif

void mpi_file_close_f(MPI_Fint *fh, MPI_Fint *ierr)
{

}
