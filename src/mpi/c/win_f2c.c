/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_f2c = PMPI_Win_f2c
#endif

MPI_Win MPI_Win_f2c(MPI_Fint win) {
    return (MPI_Win)0;
}
