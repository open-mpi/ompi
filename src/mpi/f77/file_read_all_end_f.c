/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_READ_ALL_END = mpi_file_read_all_end_f
#pragma weak pmpi_file_read_all_end = mpi_file_read_all_end_f
#pragma weak pmpi_file_read_all_end_ = mpi_file_read_all_end_f
#pragma weak pmpi_file_read_all_end__ = mpi_file_read_all_end_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_READ_ALL_END,
                           pmpi_file_read_all_end,
                           pmpi_file_read_all_end_,
                           pmpi_file_read_all_end__,
                           pmpi_file_read_all_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_READ_ALL_END = mpi_file_read_all_end_f
#pragma weak mpi_file_read_all_end = mpi_file_read_all_end_f
#pragma weak mpi_file_read_all_end_ = mpi_file_read_all_end_f
#pragma weak mpi_file_read_all_end__ = mpi_file_read_all_end_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_READ_ALL_END,
                           mpi_file_read_all_end,
                           mpi_file_read_all_end_,
                           mpi_file_read_all_end__,
                           mpi_file_read_all_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_read_all_end_f(MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr)
{

}
