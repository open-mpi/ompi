/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_free_keyval = PMPI_free_keyval
#endif

int MPI_Win_free_keyval(int *win_keyval) {
    return MPI_SUCCESS;
}
