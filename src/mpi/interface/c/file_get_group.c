/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_group = PMPI_File_get_group
#endif

int MPI_File_get_group(MPI_File fh, MPI_Group *group) {
    return MPI_SUCCESS;
}
