/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_get_name = MPI_Type_get_name
#endif

int
MPI_Type_get_name(MPI_Datatype type, char *type_name, int *resultlen)
{
    return MPI_SUCCESS;
}
