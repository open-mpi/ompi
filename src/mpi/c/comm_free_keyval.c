/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_free_keyval = PMPI_Comm_free_keyval
#endif

int MPI_Comm_free_keyval(int *comm_keyval) {
    return MPI_SUCCESS;
}
