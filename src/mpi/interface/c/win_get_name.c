/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_get_name = PMPI_win_get_name
#endif

int MPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen) {
    return MPI_SUCCESS;
}
