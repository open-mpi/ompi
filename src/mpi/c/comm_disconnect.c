/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_disconnect = PMPI_Comm_disconnect
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_disconnect";


OMPI_EXPORT
int MPI_Comm_disconnect(MPI_Comm *comm) 
{

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( MPI_COMM_NULL == *comm || ompi_comm_invalid (*comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
    }
    
    if (MPI_COMM_WORLD == *comm || MPI_COMM_SELF == *comm ) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
    }

    (*comm)->c_coll.coll_barrier(*comm);
    OBJ_RETAIN(*comm);

    *comm = MPI_COMM_NULL;
    return MPI_SUCCESS;
}
