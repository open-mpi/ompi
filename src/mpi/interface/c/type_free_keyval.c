/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_free_keyval = MPI_Type_free_keyval
#endif

int
MPI_Type_free_keyval(int *type_keyval)
{
    return MPI_SUCCESS;
}
