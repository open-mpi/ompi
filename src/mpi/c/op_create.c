/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Op_create = PMPI_Op_create
#endif

int MPI_Op_create(MPI_User_function *function, int commute,
                  MPI_Op *op) {
    return MPI_SUCCESS;
}
