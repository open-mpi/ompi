/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_processor_name = PMPI_Get_processor_name
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_processor_name";


int MPI_Get_processor_name(char *name, int *resultlen) 
{
    char tmp[MPI_MAX_PROCESSOR_NAME];
    int len;

    if ( MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ( NULL == name  ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == resultlen  ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
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
