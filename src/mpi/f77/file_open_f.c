/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_OPEN = mpi_file_open_f
#pragma weak pmpi_file_open = mpi_file_open_f
#pragma weak pmpi_file_open_ = mpi_file_open_f
#pragma weak pmpi_file_open__ = mpi_file_open_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_OPEN,
                           pmpi_file_open,
                           pmpi_file_open_,
                           pmpi_file_open__,
                           pmpi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr),
                           (comm, filename, amode, info, fh, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_OPEN = mpi_file_open_f
#pragma weak mpi_file_open = mpi_file_open_f
#pragma weak mpi_file_open_ = mpi_file_open_f
#pragma weak mpi_file_open__ = mpi_file_open_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_OPEN,
                           mpi_file_open,
                           mpi_file_open_,
                           mpi_file_open__,
                           mpi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr),
                           (comm, filename, amode, info, fh, ierr) )
#endif

void mpi_file_open_f(MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr)
{

}
