/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Close_port = PMPI_Close_port
#endif

int MPI_Close_port(char *port_name) {
    return MPI_SUCCESS;
}
