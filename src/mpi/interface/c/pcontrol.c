/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Pcontrol = PMPI_Pcontrol
#endif

int MPI_Pcontrol(const int level, ...) {
    return MPI_SUCCESS;
}

