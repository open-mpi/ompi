/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_compare = PMPI_Group_compare
#endif

int MPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result) {
    return MPI_SUCCESS;
}
