/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_set_name = PMPI_Type_set_name
#endif

int
MPI_Type_set_name (MPI_Datatype type, char *type_name)
{
    return MPI_SUCCESS;
}
