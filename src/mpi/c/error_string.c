/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errcode.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Error_string = PMPI_Error_string
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Error_string";


int MPI_Error_string(int errorcode, char *string, int *resultlen) 
{
    char *tmpstring;
    
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_mpi_errcode_is_invalid(errorcode)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }
 
    tmpstring = ompi_mpi_errcode_get_string (errorcode);
    strcpy(string, tmpstring);
    *resultlen = strlen(string);
    
    return MPI_SUCCESS;
}
