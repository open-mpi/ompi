/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_sync = PMPI_File_sync
#endif

int MPI_File_sync(MPI_File fh) {
    return MPI_SUCCESS;
}
