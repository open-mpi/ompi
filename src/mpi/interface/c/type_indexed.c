/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_indexed = MPI_Type_indexed
#endif

int
MPI_Type_indexed(int count,
                 int array_of_blocklengths[],
                 int array_of_displacements[],
                 MPI_Datatype oldtype,
                 MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
