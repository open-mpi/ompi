/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_ub = MPI_Type_ub
#endif

int
MPI_Type_ub(MPI_Datatype mtype, MPI_Aint *ub)
{
    MPI_Aint lb;
    MPI_Aint extent;
    int status;

    status = MPI_Type_get_extent(type, &lb, &extent);
    if (MPI_SUCCESS == status) {
        *ub = lb + extent;
    }

    return status;
}
