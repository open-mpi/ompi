/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Finalized = PMPI_Finalized
#endif

int MPI_Finalized(int *flag) {
    return MPI_SUCCESS;
}
