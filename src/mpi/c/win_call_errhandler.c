/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_call_errhandler = PMPI_Win_call_errhandler
#endif

int MPI_Win_call_errhandler(MPI_Win win, int errorcode) {
    return MPI_SUCCESS;
}
