/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_create_resized = MPI_Type_create_resized
#endif

int
MPI_Type_create_resized(MPI_Datatype oldtype,
                        MPI_Aint lb,
                        MPI_Aint extent,
                        MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}


