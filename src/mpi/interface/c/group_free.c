/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_free = PMPI_Group_free
#endif

int MPI_Group_free(MPI_Group *group) {
    return MPI_SUCCESS;
}
