/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_free = PMPI_Type_free
#endif

int
MPI_Type_free(MPI_Datatype *type)
{
    return MPI_SUCCESS;
}
