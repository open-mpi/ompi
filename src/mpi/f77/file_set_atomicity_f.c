/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_SET_ATOMICITY = mpi_file_set_atomicity_f
#pragma weak pmpi_file_set_atomicity = mpi_file_set_atomicity_f
#pragma weak pmpi_file_set_atomicity_ = mpi_file_set_atomicity_f
#pragma weak pmpi_file_set_atomicity__ = mpi_file_set_atomicity_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_SET_ATOMICITY,
                           pmpi_file_set_atomicity,
                           pmpi_file_set_atomicity_,
                           pmpi_file_set_atomicity__,
                           pmpi_file_set_atomicity_f,
                           (MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_ATOMICITY = mpi_file_set_atomicity_f
#pragma weak mpi_file_set_atomicity = mpi_file_set_atomicity_f
#pragma weak mpi_file_set_atomicity_ = mpi_file_set_atomicity_f
#pragma weak mpi_file_set_atomicity__ = mpi_file_set_atomicity_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_SET_ATOMICITY,
                           mpi_file_set_atomicity,
                           mpi_file_set_atomicity_,
                           mpi_file_set_atomicity__,
                           mpi_file_set_atomicity_f,
                           (MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_set_atomicity_f(MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr)
{

}
