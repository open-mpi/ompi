/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_match_size = MPI_Type_match_size
#endif

int
MPI_Type_match_size(int typeclass, int size, MPI_Datatype *type)
{
    return MPI_SUCCESS;
}
