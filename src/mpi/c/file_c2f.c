/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_c2f = PMPI_File_c2f
#endif

MPI_Fint MPI_File_c2f(MPI_File file) {
    return (MPI_Fint)0;
}
