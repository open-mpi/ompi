/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_hindexed = MPI_Type_hindexed
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
