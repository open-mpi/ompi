/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_READ = mpi_file_read_f
#pragma weak pmpi_file_read = mpi_file_read_f
#pragma weak pmpi_file_read_ = mpi_file_read_f
#pragma weak pmpi_file_read__ = mpi_file_read_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_READ,
                           pmpi_file_read,
                           pmpi_file_read_,
                           pmpi_file_read__,
                           pmpi_file_read_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, count, datatype, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_READ = mpi_file_read_f
#pragma weak mpi_file_read = mpi_file_read_f
#pragma weak mpi_file_read_ = mpi_file_read_f
#pragma weak mpi_file_read__ = mpi_file_read_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_READ,
                           mpi_file_read,
                           mpi_file_read_,
                           mpi_file_read__,
                           mpi_file_read_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, count, datatype, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_read_f(MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr)
{

}
