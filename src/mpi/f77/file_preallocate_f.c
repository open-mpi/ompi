/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_PREALLOCATE = mpi_file_preallocate_f
#pragma weak pmpi_file_preallocate = mpi_file_preallocate_f
#pragma weak pmpi_file_preallocate_ = mpi_file_preallocate_f
#pragma weak pmpi_file_preallocate__ = mpi_file_preallocate_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_PREALLOCATE,
                           pmpi_file_preallocate,
                           pmpi_file_preallocate_,
                           pmpi_file_preallocate__,
                           pmpi_file_preallocate_f,
                           (MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr),
                           (fh, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_PREALLOCATE = mpi_file_preallocate_f
#pragma weak mpi_file_preallocate = mpi_file_preallocate_f
#pragma weak mpi_file_preallocate_ = mpi_file_preallocate_f
#pragma weak mpi_file_preallocate__ = mpi_file_preallocate_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_PREALLOCATE,
                           mpi_file_preallocate,
                           mpi_file_preallocate_,
                           mpi_file_preallocate__,
                           mpi_file_preallocate_f,
                           (MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr),
                           (fh, size, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_preallocate_f(MPI_Fint *fh, MPI_Fint *size, MPI_Fint *ierr)
{

}
