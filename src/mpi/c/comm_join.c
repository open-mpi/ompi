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
#pragma weak MPI_Comm_join = PMPI_Comm_join
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_join(int fd, MPI_Comm *intercomm) 
{
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_join");
        if ( NULL == intercomm ) 
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Comm_join");  
    }
    
    /* sendrecv OOB-name (port-name) through the socket connection.
       Need to determine somehow how to avoid a potential deadlock
       here. *o/
    /* if proc unknown, set up the proc-structure */
    /* setup the intercomm-structure using ompi_comm_set (); */
    /* setup comm_cid: need a separate routine for that, 
       since it is so trivial ? How about multi-threaded case ? */
    /* PROBLEM: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ? */
    
    return MPI_SUCCESS;
}
