/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_excl = PMPI_Group_excl
#endif

int MPI_Group_excl(MPI_Group group, int n, int *ranks,
                   MPI_Group *newgroup) {
    return MPI_SUCCESS;
}
