/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_f2c = PMPI_File_f2c
#endif

MPI_File MPI_File_f2c(MPI_Fint file) {
    return (MPI_File)0;
}
