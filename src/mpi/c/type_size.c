/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_size = PMPI_Type_size
#endif

int
MPI_Type_size(MPI_Datatype type, int *size)
{
    if( type == MPI_DATATYPE_NULL ) return MPI_ERR_TYPE;
    *size = type->size;
    return MPI_SUCCESS;
}
