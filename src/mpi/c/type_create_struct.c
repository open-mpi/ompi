/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_struct = PMPI_Type_create_struct
#endif

int
MPI_Type_create_struct(int count,
                       int array_of_blocklengths[],
                       MPI_Aint array_of_displacements[],
                       MPI_Datatype array_of_types[],
                       MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
