/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Get_count = PMPI_Get_count
#endif

int MPI_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count) {
    return MPI_SUCCESS;
}
