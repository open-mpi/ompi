/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_WRITE = mpi_file_write_f
#pragma weak pmpi_file_write = mpi_file_write_f
#pragma weak pmpi_file_write_ = mpi_file_write_f
#pragma weak pmpi_file_write__ = mpi_file_write_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_WRITE,
                           pmpi_file_write,
                           pmpi_file_write_,
                           pmpi_file_write__,
                           pmpi_file_write_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, count, datatype, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_WRITE = mpi_file_write_f
#pragma weak mpi_file_write = mpi_file_write_f
#pragma weak mpi_file_write_ = mpi_file_write_f
#pragma weak mpi_file_write__ = mpi_file_write_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_WRITE,
                           mpi_file_write,
                           mpi_file_write_,
                           mpi_file_write__,
                           mpi_file_write_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, count, datatype, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_write_f(MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr)
{

}
