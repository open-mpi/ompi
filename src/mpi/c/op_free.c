/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Op_free = PMPI_Op_free
#endif

int MPI_Op_free(MPI_Op *op) {
    return MPI_SUCCESS;
}
