/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_f2c = PMPI_Group_f2c
#endif

MPI_Group MPI_Group_f2c(MPI_Fint group) {
    return (MPI_Group)0;
}
