/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_darray = PMPI_Type_create_darray
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_create_darray(int size,
                       int rank,
                       int ndims,
                       int gsize_array[],
                       int distrib_array[],
                       int darg_array[],
                       int psize_array[],
                       int order,
                       MPI_Datatype oldtype,
                       MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
