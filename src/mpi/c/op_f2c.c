/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Op_f2c = PMPI_Op_f2c
#endif

MPI_Op MPI_Op_f2c(MPI_Fint op) {
    return (MPI_Op)0;
}
