/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_DELETE = mpi_file_delete_f
#pragma weak pmpi_file_delete = mpi_file_delete_f
#pragma weak pmpi_file_delete_ = mpi_file_delete_f
#pragma weak pmpi_file_delete__ = mpi_file_delete_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_DELETE,
                           pmpi_file_delete,
                           pmpi_file_delete_,
                           pmpi_file_delete__,
                           pmpi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr),
                           (filename, info, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_DELETE = mpi_file_delete_f
#pragma weak mpi_file_delete = mpi_file_delete_f
#pragma weak mpi_file_delete_ = mpi_file_delete_f
#pragma weak mpi_file_delete__ = mpi_file_delete_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_DELETE,
                           mpi_file_delete,
                           mpi_file_delete_,
                           mpi_file_delete__,
                           mpi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr),
                           (filename, info, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_delete_f(char *filename, MPI_Fint *info, MPI_Fint *ierr)
{

}
