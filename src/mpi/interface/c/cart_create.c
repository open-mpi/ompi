/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_create = PMPI_Cart_create
#endif

int MPI_Cart_create(MPI_Comm old_comm, int ndims, int *dims,
                    int *periods, int redorder, MPI_Comm *comm_cart) {
    return MPI_SUCCESS;
}
