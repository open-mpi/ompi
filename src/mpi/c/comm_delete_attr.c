/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_delete_attr = PMPI_Comm_delete_attr
#endif

int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval) {
    return MPI_SUCCESS;
}
