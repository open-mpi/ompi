/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_subarray = PMPI_Type_create_subarray
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
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
