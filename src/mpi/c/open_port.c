/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "info/info.h"
#include "communicator/communicator.h"
#include "proc/proc.h"
#include "mca/ns/base/base.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Open_port = PMPI_Open_port
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Open_port";


int MPI_Open_port(MPI_Info info, char *port_name) 
{
    int rc;
    
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if ( NULL == port_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    if ( MPI_INFO_NULL != info ) {
        /* in theory, they user might tell us here
           how to establish the address. Since our communication
           is relying on OOB, we probably won't use the info-object.

           Potential values defined in MPI-2:
           - "ip_port"    : value contains IP port number
           - "ip_address" : value contains IP address
        */
    }

    rc = ompi_open_port(port_name);
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
