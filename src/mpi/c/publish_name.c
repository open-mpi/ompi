/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "info/info.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Publish_name = PMPI_Publish_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Publish_name(char *service_name, MPI_Info info,
                     char *port_name) 
{
    int rc;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE; 

        if ( NULL == port_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Publish_name");
        if ( NULL == service_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Publish_name");
    }

    /* 
     * No predefined info-objects for this function in MPI-2,
     * therefore, we do not parse the info-object at the moment.
     */

    rc = ompi_comm_namepublish (service_name, port_name);
    if ( OMPI_SUCCESS != rc ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN,
                                      "MPI_Publish_name");
    }
    
    return MPI_SUCCESS;
}
