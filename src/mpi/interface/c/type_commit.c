/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_commit = PMPI_Type_commit
#endif

int
MPI_Type_commit(MPI_Datatype *type)
{
    return MPI_SUCCESS;
}
