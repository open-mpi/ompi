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

static const char FUNC_NAME[] = "MPI_Comm_join";


int MPI_Comm_join(int fd, MPI_Comm *intercomm) 
{
    int rc;
    int send_first;
    ompi_communicator_t *newcomp;
    ompi_process_name_t *port_proc_name;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == intercomm ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
    }
    
    /* sendrecv OOB-name (port-name) through the socket connection.
       Need to determine somehow how to avoid a potential deadlock
       here. */

    rc = ompi_comm_connect_accept (MPI_COMM_SELF, 0, port_proc_name,
                                   send_first, &newcomp);
    
    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, MPI_COMM_SELF, rc, FUNC_NAME);
}
