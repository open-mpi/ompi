/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_get_attr = MPI_Type_get_attr
#endif

int
MPI_Type_set_attr (MPI_Datatype type,
                   int type_keyval,
                   void *attribute_val)
{
    return MPI_SUCCESS;
}
