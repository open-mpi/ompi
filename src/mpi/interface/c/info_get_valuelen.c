/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get_valuelen = PMPI_Info_get_valuelen
#endif

int MPI_Info_get_valuelen(MPI_Info info, char *key, int *valuelen,
                          int *flag) {
    return MPI_SUCCESS;
}
