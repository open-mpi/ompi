/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Status_f2c = PMPI_Status_f2c
#endif

int MPI_Status_f2c(MPI_Fint *f_status, MPI_Status *c_status) {
    return MPI_SUCCESS;
}
