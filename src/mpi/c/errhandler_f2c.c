/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_f2c = PMPI_Errhandler_f2c
#endif

MPI_Errhandler MPI_Errhandler_f2c(MPI_Fint errhandler) {
    return (MPI_Errhandler)0;
}
