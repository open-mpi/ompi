/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_dup = PMPI_Info_dup
#endif

int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo) {
    return MPI_SUCCESS;
}
