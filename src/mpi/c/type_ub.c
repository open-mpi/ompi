/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_ub = PMPI_Type_ub
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int
MPI_Type_ub(MPI_Datatype mtype, MPI_Aint *ub)
{
    MPI_Aint lb;
    MPI_Aint extent;
    int status;

    status = MPI_Type_get_extent(mtype, &lb, &extent);
    if (MPI_SUCCESS == status) {
        *ub = lb + extent;
    }

    return status;
}
