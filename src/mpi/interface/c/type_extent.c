/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_extent = MPI_Type_extent
#endif

int
MPI_Type_extent(MPI_Datatype type, MPI_Aint *extent)
{
    MPI_Aint lb;

    return MPI_Type_get_extent(type, &lb, extent);
}
