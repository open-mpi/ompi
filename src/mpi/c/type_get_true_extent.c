/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_true_extent = PMPI_Type_get_true_extent
#endif

int
MPI_Type_get_true_extent(MPI_Datatype datatype,
                         MPI_Aint *true_lb, 
                         MPI_Aint *true_extent)
{
    return MPI_SUCCESS;
}
