/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_amode = PMPI_File_get_amode
#endif

int MPI_File_get_amode(MPI_File fh, int *amode) {
    return MPI_SUCCESS;
}
