/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_READ_AT_ALL_BEGIN = mpi_file_read_at_all_begin_f
#pragma weak pmpi_file_read_at_all_begin = mpi_file_read_at_all_begin_f
#pragma weak pmpi_file_read_at_all_begin_ = mpi_file_read_at_all_begin_f
#pragma weak pmpi_file_read_at_all_begin__ = mpi_file_read_at_all_begin_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_READ_AT_ALL_BEGIN,
                           pmpi_file_read_at_all_begin,
                           pmpi_file_read_at_all_begin_,
                           pmpi_file_read_at_all_begin__,
                           pmpi_file_read_at_all_begin_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_READ_AT_ALL_BEGIN = mpi_file_read_at_all_begin_f
#pragma weak mpi_file_read_at_all_begin = mpi_file_read_at_all_begin_f
#pragma weak mpi_file_read_at_all_begin_ = mpi_file_read_at_all_begin_f
#pragma weak mpi_file_read_at_all_begin__ = mpi_file_read_at_all_begin_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_READ_AT_ALL_BEGIN,
                           mpi_file_read_at_all_begin,
                           mpi_file_read_at_all_begin_,
                           mpi_file_read_at_all_begin__,
                           mpi_file_read_at_all_begin_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_read_at_all_begin_f(MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *ierr)
{

}
