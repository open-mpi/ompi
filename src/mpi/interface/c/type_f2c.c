/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_f2c.c = PMPI_Type_f2c.c
#endif

MPI_Datatype MPI_Type_f2c(MPI_Fint datatype) {
    return (MPI_Datatype)0;
}
