/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_attr = PMPI_Type_get_attr
#endif

int
MPI_Type_set_attr (MPI_Datatype type,
                   int type_keyval,
                   void *attribute_val)
{
    return MPI_SUCCESS;
}
