/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_CALL_ERRHANDLER = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler_ = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler__ = mpi_file_call_errhandler_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_CALL_ERRHANDLER,
                           pmpi_file_call_errhandler,
                           pmpi_file_call_errhandler_,
                           pmpi_file_call_errhandler__,
                           pmpi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CALL_ERRHANDLER = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler_ = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler__ = mpi_file_call_errhandler_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_CALL_ERRHANDLER,
                           mpi_file_call_errhandler,
                           mpi_file_call_errhandler_,
                           mpi_file_call_errhandler__,
                           mpi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_call_errhandler_f(MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr)
{

}
