/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Request_c2f = PMPI_Request_c2f
#endif

MPI_Fint MPI_Request_c2f(MPI_Request request) {
    return (MPI_Fint)0;
}
