/*
 *  * $HEADER$
 *   */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Add_error_code = PMPI_Add_error_code
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Add_error_code(int errorclass, int *errorcode)
{
    int code;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE;

        if ( ompi_errclass_is_invalid(errorclass) )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Add_error_code");
        
    }
    
    code = ompi_mpi_errcode_add ( errorclass);
    if ( 0 > code ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN,
                                      "MPI_Add_error_code");
    }
    
    *errorcode = code;
    return MPI_SUCCESS;
}
