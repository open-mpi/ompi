/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_errhandler = PMPI_File_get_errhandler
#endif

int MPI_File_get_errhandler( MPI_File file, MPI_Errhandler *errhandler) {
    return MPI_SUCCESS;
}
