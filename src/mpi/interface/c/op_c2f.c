/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Op_c2f = PMPI_Op_c2f
#endif

MPI_Fint MPI_Op_c2f(MPI_Op op) {
    return (MPI_Fint)0;
}
