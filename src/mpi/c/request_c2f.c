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


OMPI_EXPORT
MPI_Fint MPI_Request_c2f(MPI_Request request) 
{
    /* local variables */
    ompi_request_t *request_c;

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

 	    /* mapping an invalid handle to a null handle */
	    /* not invoking an error handler */
        if( (NULL == request) ) {
			request = MPI_REQUEST_NULL;
        }
    }

    request_c=(ompi_request_t *)request;

    return (MPI_Fint) (request_c->req_f_to_c_index) ;
}
