/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_set_atomicity = PMPI_File_set_atomicity
#endif

int MPI_File_set_atomicity(MPI_File fh, int flag) {
    return MPI_SUCCESS;
}
