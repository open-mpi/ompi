/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WAIT = mpi_wait_f
#pragma weak pmpi_wait = mpi_wait_f
#pragma weak pmpi_wait_ = mpi_wait_f
#pragma weak pmpi_wait__ = mpi_wait_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAIT,
                           pmpi_wait,
                           pmpi_wait_,
                           pmpi_wait__,
                           pmpi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAIT = mpi_wait_f
#pragma weak mpi_wait = mpi_wait_f
#pragma weak mpi_wait_ = mpi_wait_f
#pragma weak mpi_wait__ = mpi_wait_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAIT,
                           mpi_wait,
                           mpi_wait_,
                           mpi_wait__,
                           mpi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_wait_f(MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Status  c_status;

    *ierr = MPI_Wait(&c_req, &c_status);

    if (*ierr == MPI_SUCCESS) {
        /* reset request handle to MPI_REQUEST_NULL */
        *request = -1;
        MPI_Status_c2f(&c_status, status);
    }

}
