/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_delete = PMPI_File_delete
#endif

int MPI_File_delete(char *filename, MPI_Info info) {
    return MPI_SUCCESS;
}
