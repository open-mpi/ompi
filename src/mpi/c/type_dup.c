/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_dup = PMPI_Type_dup
#endif

int
MPI_Type_dup (MPI_Datatype type,
              MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
