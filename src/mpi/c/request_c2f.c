/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "request/request.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Request_c2f = PMPI_Request_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Request_f2c";


MPI_Fint MPI_Request_c2f(MPI_Request request) 
{
    /* error checking */
    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* mapping an invalid handle to a null handle */
        /* not invoking an error handler */

        if (NULL == request) {
            request = MPI_REQUEST_NULL;
        }
    }

    /* We only put requests in the f2c table when this function is
       invoked.  This is because putting requests in the table
       involves locking and unlocking the table, which would incur a
       performance penalty (in the critical performance path) for C
       applications.  In this way, at least only Fortran applications
       are penalized.  :-\

       Modifying this one function neatly fixes up all the Fortran
       bindings because they all call MPI_Request_c2f in order to
       transmorgify the C MPI_Request that they got back into a
       fortran integer.
    */

    if (-1 == request->req_f_to_c_index) {
        request->req_f_to_c_index = 
            ompi_pointer_array_add(&ompi_request_f_to_c_table, request);
    }

    return (MPI_Fint) (request->req_f_to_c_index) ;
}
