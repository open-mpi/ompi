/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_set_name = PMPI_Win_set_name
#endif

int MPI_Win_set_name(MPI_Win win, char *win_name) {
    return MPI_SUCCESS;
}
