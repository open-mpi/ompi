/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_get_true_extent = MPI_Type_get_true_extent
#endif

int
MPI_Type_get_true_extent(MPI_Datatype datatype,
                         MPI_Aint *true_lb, 
                         MPI_Aint *true_extent)
{
    return MPI_SUCCESS;
}
