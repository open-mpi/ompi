/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_difference = PMPI_Group_difference
#endif

int MPI_Group_difference(MPI_Group group1, MPI_Group group2,
                         MPI_Group *newgroup) {
    return MPI_SUCCESS;
}
