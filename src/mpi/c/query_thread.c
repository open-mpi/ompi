/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Query_thread = PMPI_Query_thread
#endif

int MPI_Query_thread(int *provided) {
    return MPI_SUCCESS;
}
