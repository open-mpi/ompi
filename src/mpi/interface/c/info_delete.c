/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_delete = PMPI_Info_delete
#endif

int MPI_Info_delete(MPI_Info info, char *key) {
    return MPI_SUCCESS;
}
