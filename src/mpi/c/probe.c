/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Probe = PMPI_Probe
#endif

int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status) {
    return MPI_SUCCESS;
}
