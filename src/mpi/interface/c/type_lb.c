/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_lb = MPI_Type_lb
#endif

int
MPI_Type_lb(MPI_Datatype type, MPI_Aint *lb)
{
    MPI_Aint extent;

    return MPI_Type_get_extent(type, lb, &extent);
}
