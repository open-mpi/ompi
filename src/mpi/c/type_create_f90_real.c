/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_real = PMPI_Type_create_f90_real
#endif

int
MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
