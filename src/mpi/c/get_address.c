/*
 * $HEADERS$
 */
#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Get_address = PMPI_Get_address
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Get_address";

int MPI_Get_address(void *location, MPI_Aint *address)
{
    if( MPI_PARAM_CHECK ) {
        if( LAM_MPI_INVALID_STATE ) {
            LAM_ERRHANDLER_RETURN( MPI_ERR_INTERN, (lam_communicator_t*)NULL,
                                   MPI_ERR_INTERN, FUNC_NAME );
        }
    }
    *address = (MPI_Aint)location;
    return MPI_SUCCESS;
}
