/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_hindexed = PMPI_Type_hindexed
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_hindexed(int count,
                  int array_of_blocklengths[],
                  MPI_Aint array_of_displacements[],
                  MPI_Datatype oldtype,
                  MPI_Datatype *newtype)
{
    return MPI_Type_create_hindexed(count,
                                    array_of_blocklengths,
                                    array_of_displacements,
                                    oldtype,
                                    newtype);
}
