/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "info/info.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Lookup_name = PMPI_Lookup_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Lookup_name(char *service_name, MPI_Info info, char *port_name) 
{
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Lookup_name");
        if ( NULL == port_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Lookup_name");
        if ( NULL == service_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          "MPI_Lookup_name");
    }

    /* parse info-object. No predefined info-objects for this function
       in MPI-2 */
    /* retrieve information from registry using service_name as a key */
    /* copy data into port_name, if found */
       
    return MPI_SUCCESS;
}
