/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Bsend = PMPI_Bsend
#endif


int MPI_Bsend(void *buf, int count, MPI_Datatype datatype,
                          int dest, int tag, MPI_Comm comm) {
    return MPI_SUCCESS;
}
