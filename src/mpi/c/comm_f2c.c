/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_f2c = PMPI_Comm_f2c
#endif


MPI_Comm MPI_Comm_f2c(MPI_Fint comm) {
    /*
     * Anju:
     * Check if what I have returned is right
     */
    return MPI_COMM_NULL;
}
