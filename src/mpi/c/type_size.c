/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_size = PMPI_Type_size
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Type_size";

int
MPI_Type_size(MPI_Datatype type, int *size)
{
    if( type == MPI_DATATYPE_NULL ) {
        OMPI_ERRHANDLER_RETURN( MPI_ERR_TYPE, MPI_COMM_WORLD,
                               MPI_ERR_TYPE, FUNC_NAME );
	}
    *size = type->size;
    return MPI_SUCCESS;
}
