/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Init_thread = PMPI_Init_thread
#endif

int MPI_Init_thread(int *argc, char ***argv, int required,
                    int *provided) {
    return MPI_SUCCESS;
}
