/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_name = PMPI_Type_get_name
#endif

int
MPI_Type_get_name(MPI_Datatype type, char *type_name, int *resultlen)
{
    return MPI_SUCCESS;
}
