/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_resized = PMPI_Type_create_resized
#endif

int
MPI_Type_create_resized(MPI_Datatype oldtype,
                        MPI_Aint lb,
                        MPI_Aint extent,
                        MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}


