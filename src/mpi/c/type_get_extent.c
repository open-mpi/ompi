/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_extent = PMPI_Type_get_extent
#endif

int
MPI_Type_get_extent(MPI_Datatype type, MPI_Aint *lb, MPI_Aint *extent)
{
    return MPI_SUCCESS;
}
