/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_position_shared = PMPI_File_get_position_shared
#endif

int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset) {
    return MPI_SUCCESS;
}
