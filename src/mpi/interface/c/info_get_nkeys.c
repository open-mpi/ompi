/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nkeys = PMPI_Info_get_nkeys
#endif

int MPI_Info_get_nkeys(MPI_Info info, int *nkeys) {
    return MPI_SUCCESS;
}
