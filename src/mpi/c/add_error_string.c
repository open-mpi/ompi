/*
 *  *  * $HEADER$
 *   *   */
#include "ompi_config.h"
#include <stdio.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errcode.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Add_error_string = PMPI_Add_error_string
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Add_error_string(int errorcode, char *string)
{
    int rc;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized) 
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN,
                                          "MPI_Add_error_string");
        if ( ompi_mpi_errcode_is_invalid(errorcode) )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Add_error_string");
    }

    rc = ompi_mpi_errcode_add_string (errorcode, string, strlen(string)+1);
    if ( OMPI_SUCCESS != rc ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN,
                                      "MPI_Add_error_string");
    }

    return MPI_SUCCESS;
}
