/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REQUEST_FREE = mpi_request_free_f
#pragma weak pmpi_request_free = mpi_request_free_f
#pragma weak pmpi_request_free_ = mpi_request_free_f
#pragma weak pmpi_request_free__ = mpi_request_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_FREE,
                           pmpi_request_free,
                           pmpi_request_free_,
                           pmpi_request_free__,
                           pmpi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_FREE = mpi_request_free_f
#pragma weak mpi_request_free = mpi_request_free_f
#pragma weak mpi_request_free_ = mpi_request_free_f
#pragma weak mpi_request_free__ = mpi_request_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_FREE,
                           mpi_request_free,
                           mpi_request_free_,
                           mpi_request_free__,
                           mpi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_request_free_f(MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c( *request ); 

    *ierr = MPI_Request_free( &c_req );

    *request = (MPI_Fint)MPI_REQUEST_NULL;
}
