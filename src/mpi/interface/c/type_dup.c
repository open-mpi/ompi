/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_dup = MPI_Type_dup
#endif

int
MPI_Type_dup (MPI_Datatype type,
              MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
