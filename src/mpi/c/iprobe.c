/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Iprobe = PMPI_Iprobe
#endif

int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag,
               MPI_Status *status) {
    return MPI_SUCCESS;
}
