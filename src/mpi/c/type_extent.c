/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_extent = PMPI_Type_extent
#endif

int
MPI_Type_extent(MPI_Datatype type, MPI_Aint *extent)
{
    MPI_Aint lb;

    return MPI_Type_get_extent(type, &lb, extent);
}
