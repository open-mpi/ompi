/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_disconnect = PMPI_Comm_disconnect
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Comm_disconnect";


int MPI_Comm_disconnect(MPI_Comm *comm) 
{

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( MPI_COMM_NULL == *comm || ompi_comm_invalid (*comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
    }
    
    /* disconnect means, just decrease the refcount, without calling
       attribute delete fnct. etc. (to be verified.).
       
       Question: do we need a flag verifying which communicators
       we are allowed to disconnect from ? E.g. what happens,
       if we disconnect from an MPI-1 communicator derived
       from MPI_COMM_WORLD ? */
    OBJ_RETAIN(*comm);

    *comm = MPI_COMM_NULL;
    return MPI_SUCCESS;
}
