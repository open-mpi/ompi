/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_WRITE_ALL_BEGIN = mpi_file_write_all_begin_f
#pragma weak pmpi_file_write_all_begin = mpi_file_write_all_begin_f
#pragma weak pmpi_file_write_all_begin_ = mpi_file_write_all_begin_f
#pragma weak pmpi_file_write_all_begin__ = mpi_file_write_all_begin_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_WRITE_ALL_BEGIN,
                           pmpi_file_write_all_begin,
                           pmpi_file_write_all_begin_,
                           pmpi_file_write_all_begin__,
                           pmpi_file_write_all_begin_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr),
                           (fh, buf, count, datatype, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_WRITE_ALL_BEGIN = mpi_file_write_all_begin_f
#pragma weak mpi_file_write_all_begin = mpi_file_write_all_begin_f
#pragma weak mpi_file_write_all_begin_ = mpi_file_write_all_begin_f
#pragma weak mpi_file_write_all_begin__ = mpi_file_write_all_begin_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_WRITE_ALL_BEGIN,
                           mpi_file_write_all_begin,
                           mpi_file_write_all_begin_,
                           mpi_file_write_all_begin__,
                           mpi_file_write_all_begin_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr),
                           (fh, buf, count, datatype, ierr) )
#endif

void mpi_file_write_all_begin_f(MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr)
{

}
