/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_IREAD_SHARED = mpi_file_iread_shared_f
#pragma weak pmpi_file_iread_shared = mpi_file_iread_shared_f
#pragma weak pmpi_file_iread_shared_ = mpi_file_iread_shared_f
#pragma weak pmpi_file_iread_shared__ = mpi_file_iread_shared_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_IREAD_SHARED,
                           pmpi_file_iread_shared,
                           pmpi_file_iread_shared_,
                           pmpi_file_iread_shared__,
                           pmpi_file_iread_shared_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, buf, count, datatype, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_IREAD_SHARED = mpi_file_iread_shared_f
#pragma weak mpi_file_iread_shared = mpi_file_iread_shared_f
#pragma weak mpi_file_iread_shared_ = mpi_file_iread_shared_f
#pragma weak mpi_file_iread_shared__ = mpi_file_iread_shared_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_IREAD_SHARED,
                           mpi_file_iread_shared,
                           mpi_file_iread_shared_,
                           mpi_file_iread_shared__,
                           mpi_file_iread_shared_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, buf, count, datatype, request, ierr) )
#endif

void mpi_file_iread_shared_f(MPI_Fint *fh, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr)
{

}
