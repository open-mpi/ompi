/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_size = PMPI_Type_size
#endif

int
MPI_Type_size(MPI_Datatype type, int *size)
{
    return MPI_SUCCESS;
}
