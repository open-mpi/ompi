/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Rsend = PMPI_Rsend
#endif

int MPI_Rsend(void *ibuf, int count, MPI_Datatype datatype, int dest,
              int tag, MPI_Comm comm) {
    return MPI_SUCCESS;
}
