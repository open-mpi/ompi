/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WAITANY = mpi_waitany_f
#pragma weak pmpi_waitany = mpi_waitany_f
#pragma weak pmpi_waitany_ = mpi_waitany_f
#pragma weak pmpi_waitany__ = mpi_waitany_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITANY,
                           pmpi_waitany,
                           pmpi_waitany_,
                           pmpi_waitany__,
                           pmpi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITANY = mpi_waitany_f
#pragma weak mpi_waitany = mpi_waitany_f
#pragma weak mpi_waitany_ = mpi_waitany_f
#pragma weak mpi_waitany__ = mpi_waitany_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITANY,
                           mpi_waitany,
                           mpi_waitany_,
                           mpi_waitany__,
                           mpi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_waitany_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i;

    c_req = malloc(*count * sizeof(MPI_Request));
    if (c_req == NULL) {
        *ierr = MPI_ERR_INTERN;
        free(c_req);
        return;
    }

    for (i = 0; i < *count; i++) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = MPI_Waitany(*count, c_req, index, &c_status);

    if (*ierr == MPI_SUCCESS) {
        /*
         * Increment index by one for fortran conventions
         */
        if (*index != MPI_UNDEFINED) {
            *index += 1;
        }
        MPI_Status_c2f( &c_status, status); 
    }
    free(c_req);
}
