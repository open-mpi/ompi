/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_set_attribute = PMPI_Comm_set_attribute
#endif

int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val) {
    return MPI_SUCCESS;
}
