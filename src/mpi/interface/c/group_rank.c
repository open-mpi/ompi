/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_rank = PMPI_Group_rank
#endif

int MPI_Group_rank(MPI_Group group, int *rank) {
    return MPI_SUCCESS;
}
