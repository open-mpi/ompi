/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_get_errhandler = PMPI_Win_get_errhandler
#endif

int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler) {
    return MPI_SUCCESS;
}
