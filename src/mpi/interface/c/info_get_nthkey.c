/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_nthkey = PMPI_Info_get_nthkey
#endif

int MPI_Info_get_nthkey(MPI_Info info, int n, char *key) {
    return MPI_SUCCESS;
}
