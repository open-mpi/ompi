/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_set = PMPI_Info_set
#endif

int MPI_Info_set(MPI_Info info, char *key, char *value) {
    return MPI_SUCCESS;
}
