/*
 * $HEADERS$
 */
#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_address = PMPI_Get_address
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_address";

int MPI_Get_address(void *location, MPI_Aint *address)
{
    if( MPI_PARAM_CHECK ) {
        if( OMPI_MPI_INVALID_STATE ) {
            OMPI_ERRHANDLER_RETURN( MPI_ERR_INTERN, MPI_COMM_WORLD,
                                   MPI_ERR_INTERN, FUNC_NAME );
        }
    }
    *address = (MPI_Aint)location;
    return MPI_SUCCESS;
}
