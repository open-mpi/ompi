/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_set_name = MPI_Type_set_name
#endif

int
MPI_Type_set_name (MPI_Datatype type, char *type_name)
{
    return MPI_SUCCESS;
}
