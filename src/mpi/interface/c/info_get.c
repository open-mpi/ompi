/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_get = PMPI_Info_get
#endif

int MPI_Info_get(MPI_Info info, char *key, int valuelen,
                 char *value, int *flag) {
    return MPI_SUCCESS;
}
