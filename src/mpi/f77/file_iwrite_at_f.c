/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_IWRITE_AT = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at_ = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at__ = mpi_file_iwrite_at_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_IWRITE_AT,
                           pmpi_file_iwrite_at,
                           pmpi_file_iwrite_at_,
                           pmpi_file_iwrite_at__,
                           pmpi_file_iwrite_at_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_IWRITE_AT = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at_ = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at__ = mpi_file_iwrite_at_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_IWRITE_AT,
                           mpi_file_iwrite_at,
                           mpi_file_iwrite_at_,
                           mpi_file_iwrite_at__,
                           mpi_file_iwrite_at_f,
                           (MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_iwrite_at_f(MPI_Fint *fh, MPI_Fint *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr)
{

}
