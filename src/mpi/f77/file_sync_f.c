/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_SYNC = mpi_file_sync_f
#pragma weak pmpi_file_sync = mpi_file_sync_f
#pragma weak pmpi_file_sync_ = mpi_file_sync_f
#pragma weak pmpi_file_sync__ = mpi_file_sync_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_SYNC,
                           pmpi_file_sync,
                           pmpi_file_sync_,
                           pmpi_file_sync__,
                           pmpi_file_sync_f,
                           (MPI_Fint *fh, MPI_Fint *ierr),
                           (fh, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SYNC = mpi_file_sync_f
#pragma weak mpi_file_sync = mpi_file_sync_f
#pragma weak mpi_file_sync_ = mpi_file_sync_f
#pragma weak mpi_file_sync__ = mpi_file_sync_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_SYNC,
                           mpi_file_sync,
                           mpi_file_sync_,
                           mpi_file_sync__,
                           mpi_file_sync_f,
                           (MPI_Fint *fh, MPI_Fint *ierr),
                           (fh, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_sync_f(MPI_Fint *fh, MPI_Fint *ierr)
{

}
