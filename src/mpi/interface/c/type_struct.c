/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_struct = MPI_Type_struct
#endif

int
MPI_Type_struct(int count,
                int array_of_blocklengths[],
                MPI_Aint array_of_displacements[],
                MPI_Datatype array_of_types[],
                MPI_Datatype *newtype)
{
    return MPI_Type_create_struct(count,
                                  array_of_blocklengths,
                                  array_of_displacements,
                                  array_of_types,
                                  newtype);
}
