/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_hindexed = PMPI_Type_create_hindexed
#endif

int
MPI_Type_create_hindexed(int count,
                         int array_of_blocklengths[],
                         MPI_Aint array_of_displacements[],
                         MPI_Datatype oldtype,
                         MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
