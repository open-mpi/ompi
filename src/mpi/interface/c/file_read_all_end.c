/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_read_all_end = PMPI_File_read_all_end
#endif

int MPI_File_read_all_end(MPI_File fh, void *buf, MPI_Status *status) {
    return MPI_SUCCESS;
}
