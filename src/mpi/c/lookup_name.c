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

static const char FUNC_NAME[] = "MPI_Lookup_name";


int MPI_Lookup_name(char *service_name, MPI_Info info, char *port_name) 
{
    char *tmp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if ( NULL == port_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == service_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /* 
     * No predefined info-objects for this function in MPI-2,
     * therefore, we do not parse the info-object at the moment.
     */

    /*
     * if multiple entries found, this implementation uses
     * at the moment the first entry.
     */
    tmp = (char *) ompi_comm_namelookup(service_name);
    if ( NULL == tmp ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NAME,
                                      FUNC_NAME);
    }

    strncpy ( port_name, tmp, MPI_MAX_PORT_NAME );    
    return MPI_SUCCESS;
}
