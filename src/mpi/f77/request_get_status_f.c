/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_REQUEST_GET_STATUS = mpi_request_get_status_f
#pragma weak pmpi_request_get_status = mpi_request_get_status_f
#pragma weak pmpi_request_get_status_ = mpi_request_get_status_f
#pragma weak pmpi_request_get_status__ = mpi_request_get_status_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_REQUEST_GET_STATUS,
                           pmpi_request_get_status,
                           pmpi_request_get_status_,
                           pmpi_request_get_status__,
                           pmpi_request_get_status_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_GET_STATUS = mpi_request_get_status_f
#pragma weak mpi_request_get_status = mpi_request_get_status_f
#pragma weak mpi_request_get_status_ = mpi_request_get_status_f
#pragma weak mpi_request_get_status__ = mpi_request_get_status_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_REQUEST_GET_STATUS,
                           mpi_request_get_status,
                           mpi_request_get_status_,
                           mpi_request_get_status__,
                           mpi_request_get_status_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_request_get_status_f(MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{

}
