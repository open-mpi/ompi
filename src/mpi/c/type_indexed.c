/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_indexed = PMPI_Type_indexed
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
