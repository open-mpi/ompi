/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_sub = PMPI_Cart_sub
#endif


int MPI_Cart_sub(MPI_Comm comm, int *remain_dims, MPI_Comm *new_comm) {
    return MPI_SUCCESS;
}
