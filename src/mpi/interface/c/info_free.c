/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_free = PMPI_Info_free
#endif

int MPI_Info_free(MPI_Info *info) {
    return MPI_SUCCESS;
}
