/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_set_attr = PMPI_Win_set_attr
#endif

int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val) {
    return MPI_SUCCESS;
}
