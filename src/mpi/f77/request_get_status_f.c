/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REQUEST_GET_STATUS = mpi_request_get_status_f
#pragma weak pmpi_request_get_status = mpi_request_get_status_f
#pragma weak pmpi_request_get_status_ = mpi_request_get_status_f
#pragma weak pmpi_request_get_status__ = mpi_request_get_status_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_GET_STATUS,
                           pmpi_request_get_status,
                           pmpi_request_get_status_,
                           pmpi_request_get_status__,
                           pmpi_request_get_status_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_GET_STATUS = mpi_request_get_status_f
#pragma weak mpi_request_get_status = mpi_request_get_status_f
#pragma weak mpi_request_get_status_ = mpi_request_get_status_f
#pragma weak mpi_request_get_status__ = mpi_request_get_status_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_GET_STATUS,
                           mpi_request_get_status,
                           mpi_request_get_status_,
                           mpi_request_get_status__,
                           mpi_request_get_status_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_request_get_status_f(MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Status c_status;
    MPI_Request c_req = MPI_Request_f2c( *request ); 

    *ierr = MPI_Request_get_status( c_req, flag, &c_status );

    MPI_Status_c2f( &c_status, status );
}
