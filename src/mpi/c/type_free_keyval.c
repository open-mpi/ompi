/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_free_keyval = PMPI_Type_free_keyval
#endif

int
MPI_Type_free_keyval(int *type_keyval)
{
    return MPI_SUCCESS;
}
