/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_size = MPI_Type_size
#endif

int
MPI_Type_size(MPI_Datatype type, int *size)
{
    return MPI_SUCCESS;
}
