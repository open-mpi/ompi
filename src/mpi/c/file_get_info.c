/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_info = PMPI_File_get_info
#endif

int MPI_File_get_info(MPI_File fh, MPI_Info *info_used) {
    return MPI_SUCCESS;
}
