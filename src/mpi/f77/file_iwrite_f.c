/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_IWRITE = mpi_file_iwrite_f
#pragma weak pmpi_file_iwrite = mpi_file_iwrite_f
#pragma weak pmpi_file_iwrite_ = mpi_file_iwrite_f
#pragma weak pmpi_file_iwrite__ = mpi_file_iwrite_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_IWRITE,
                           pmpi_file_iwrite,
                           pmpi_file_iwrite_,
                           pmpi_file_iwrite__,
                           pmpi_file_iwrite_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, buf, count, datatype, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_IWRITE = mpi_file_iwrite_f
#pragma weak mpi_file_iwrite = mpi_file_iwrite_f
#pragma weak mpi_file_iwrite_ = mpi_file_iwrite_f
#pragma weak mpi_file_iwrite__ = mpi_file_iwrite_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_IWRITE,
                           mpi_file_iwrite,
                           mpi_file_iwrite_,
                           mpi_file_iwrite__,
                           mpi_file_iwrite_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, buf, count, datatype, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_iwrite_f(MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr)
{

}
