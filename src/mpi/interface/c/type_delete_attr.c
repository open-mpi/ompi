/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_delete_attr = MPI_Type_delete_attr
#endif

int
MPI_Type_delete_attr (MPI_Datatype type, int type_keyval)
{
    return MPI_SUCCESS;
}
