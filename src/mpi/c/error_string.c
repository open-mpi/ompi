/*
 * $HEADER$
 */
#include <string.h>
#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errcode.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Error_string = PMPI_Error_string
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Error_string(int errorcode, char *string, int *resultlen) 
{
    char *tmpstring;
    
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE;

        if ( ompi_mpi_errcode_is_invalid(errorcode))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Error_string");
    }
 
    tmpstring = ompi_mpi_errcode_get_string (errorcode);
    strcpy(string, tmpstring);
    *resultlen = strlen(string);
    
    return MPI_SUCCESS;
}
