/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_contiguous = MPI_Type_contiguous
#endif

int
MPI_Type_contiguous(int count,
                    MPI_Datatype oldtype,
                    MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
