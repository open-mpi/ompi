/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_create_hvector = MPI_Type_create_hvector
#endif

int
MPI_Type_create_hvector(int count,
                        int blocklength,
                        MPI_Aint stride,
                        MPI_Datatype oldtype,
                        MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
