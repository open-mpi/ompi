/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_set_size = PMPI_File_set_size
#endif

int MPI_File_set_size(MPI_File fh, MPI_Offset size) {
    return MPI_SUCCESS;
}
