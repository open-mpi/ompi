/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_close = PMPI_File_close
#endif

int MPI_File_close(MPI_File *fh) {
    return MPI_SUCCESS;
}
