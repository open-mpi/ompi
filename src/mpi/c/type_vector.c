/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_vector = PMPI_Type_vector
#endif

int
MPI_Type_vector(int count,
                int blocklength,
                int stride,
                MPI_Datatype oldtype,
                MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
