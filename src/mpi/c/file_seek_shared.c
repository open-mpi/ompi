/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_seek_shared = PMPI_File_seek_shared
#endif

int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence) {
    return MPI_SUCCESS;
}
