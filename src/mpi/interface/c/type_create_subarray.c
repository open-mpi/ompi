/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_create_subarray = MPI_Type_create_subarray
#endif

int
MPI_Type_create_subarray(int ndims,
                         int size_array[],
                         int subsize_array[],
                         int start_array[],
                         int order,
                         MPI_Datatype oldtype,
                         MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
