/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Close_port = PMPI_Close_port
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Close_port";


OMPI_EXPORT
int MPI_Close_port(char *port_name) 
{
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == port_name )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
    }

    /* 
     *  since the port_name is our own process_name_t structure,
     *  we do not have to close anything or free a pointer.
     *  This function is therefore just a dummy function
     *  and fully implemented. I love these type functions,
     *  we should have more of them :-). 
     */

    return MPI_SUCCESS;
}
