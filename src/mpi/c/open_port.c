/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "info/info.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Open_port = PMPI_Open_port
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Open_port(MPI_Info info, char *port_name) 
{
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Open_port");
        if ( NULL == port_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Open_port");
    }

    if ( MPI_INFO_NULL != info ) {
        /* in theory, they user might tell us here
           how to establish the address. Since our communication
           is relying on OOB, we probably
           won't use the info-object.

           potential values defined in MPI-2:
           - "ip_port" : value contains IP port number
           - "ip_address" : value contains IP address
        */
    }

    /* According to our current understanding, the port_name will
       be the OOB-name. No real port has to be opne, since
       OOB always accepts new connections (e.g. has a pending accept).
       
       memcpy ( port_name, oob-whatever-fcnt, strlen(oob-whatever-fctn));
    */

    return MPI_SUCCESS;
}
