/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_preallocate = PMPI_File_preallocate
#endif

int MPI_File_preallocate(MPI_File fh, MPI_Offset size) {
    return MPI_SUCCESS;
}
