/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_AMODE = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode_ = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode__ = mpi_file_get_amode_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_GET_AMODE,
                           pmpi_file_get_amode,
                           pmpi_file_get_amode_,
                           pmpi_file_get_amode__,
                           pmpi_file_get_amode_f,
                           (MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr),
                           (fh, amode, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_AMODE = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode_ = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode__ = mpi_file_get_amode_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_GET_AMODE,
                           mpi_file_get_amode,
                           mpi_file_get_amode_,
                           mpi_file_get_amode__,
                           mpi_file_get_amode_f,
                           (MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr),
                           (fh, amode, ierr) )
#endif

void mpi_file_get_amode_f(MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr)
{

}
