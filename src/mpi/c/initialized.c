/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Initialized = PMPI_Initialized
#endif

int MPI_Initialized(int *flag) {
    return MPI_SUCCESS;
}
