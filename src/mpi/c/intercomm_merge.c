/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Intercomm_merge = PMPI_Intercomm_merge
#endif

int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newintercomm) {
    return MPI_SUCCESS;
}
