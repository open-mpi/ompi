/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_processor_name = PMPI_Get_processor_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Get_processor_name(char *name, int *resultlen) 
{
    char tmp[MPI_MAX_PROCESSOR_NAME];
    int len;

    if ( MPI_PARAM_CHECK) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Get_processor_name");
        if ( NULL == name  )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                         "MPI_Get_processor_name");
        if ( NULL == resultlen  )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                         "MPI_Get_processor_name");
    }
    
    /* A simple implementation of this function using gethostname*/
    gethostname (tmp, MPI_MAX_PROCESSOR_NAME);
    len = strlen (tmp);
    strncpy ( name, tmp, len);

    if ( MPI_MAX_PROCESSOR_NAME > len ) {
        *resultlen = len;
        name[len] = '\0';
    }
    else {
        *resultlen = MPI_MAX_PROCESSOR_NAME-1;
        name[MPI_MAX_PROCESSOR_NAME-1] = '\0';
    }

    return MPI_SUCCESS;
}
