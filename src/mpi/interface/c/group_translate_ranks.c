/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_translate_ranks = PMPI_Group_translate_ranks
#endif

int MPI_Group_translate_ranks(MPI_Group group1, int n, int *ranks1,
                              MPI_Group group2, int *ranks2) {
    return MPI_SUCCESS;
}
