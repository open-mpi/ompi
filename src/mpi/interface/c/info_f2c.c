/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#endif

MPI_Info MPI_Info_f2c(MPI_Fint info) {
    return (MPI_Info)0;
}
