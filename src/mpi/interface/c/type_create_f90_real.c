/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_create_f90_real = MPI_Type_create_f90_real
#endif

int
MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
