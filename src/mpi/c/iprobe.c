/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Iprobe = PMPI_Iprobe
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag,
               MPI_Status *status) {
    return MPI_SUCCESS;
}
