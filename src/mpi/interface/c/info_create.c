/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_create = PMPI_Info_create
#endif

int MPI_Info_create(MPI_Info *info) {
    return MPI_SUCCESS;
}
