/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_integer = PMPI_Type_create_f90_integer
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
