/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_create_f90_integer = MPI_Type_create_f90_integer
#endif

int
MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
