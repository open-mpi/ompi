/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_delete_attr = PMPI_Type_delete_attr
#endif

int
MPI_Type_delete_attr (MPI_Datatype type, int type_keyval)
{
    return MPI_SUCCESS;
}
