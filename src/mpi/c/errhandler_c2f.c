/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_c2f = PMPI_Errhandler_c2f
#endif

MPI_Fint MPI_Errhandler_c2f(MPI_Errhandler errhandler) {
    return (MPI_Fint)0;
}
