/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_IREAD_AT = mpi_file_iread_at_f
#pragma weak pmpi_file_iread_at = mpi_file_iread_at_f
#pragma weak pmpi_file_iread_at_ = mpi_file_iread_at_f
#pragma weak pmpi_file_iread_at__ = mpi_file_iread_at_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_IREAD_AT,
                           pmpi_file_iread_at,
                           pmpi_file_iread_at_,
                           pmpi_file_iread_at__,
                           pmpi_file_iread_at_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_IREAD_AT = mpi_file_iread_at_f
#pragma weak mpi_file_iread_at = mpi_file_iread_at_f
#pragma weak mpi_file_iread_at_ = mpi_file_iread_at_f
#pragma weak mpi_file_iread_at__ = mpi_file_iread_at_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_IREAD_AT,
                           mpi_file_iread_at,
                           mpi_file_iread_at_,
                           mpi_file_iread_at__,
                           mpi_file_iread_at_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_iread_at_f(MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
