/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WAITALL = mpi_waitall_f
#pragma weak pmpi_waitall = mpi_waitall_f
#pragma weak pmpi_waitall_ = mpi_waitall_f
#pragma weak pmpi_waitall__ = mpi_waitall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITALL,
                           pmpi_waitall,
                           pmpi_waitall_,
                           pmpi_waitall__,
                           pmpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITALL = mpi_waitall_f
#pragma weak mpi_waitall = mpi_waitall_f
#pragma weak mpi_waitall_ = mpi_waitall_f
#pragma weak mpi_waitall__ = mpi_waitall_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITALL,
                           mpi_waitall,
                           mpi_waitall_,
                           mpi_waitall__,
                           mpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_waitall_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i;

    c_req = malloc(*count * sizeof(MPI_Request));
    if (c_req == NULL) {
        *ierr = MPI_ERR_INTERN;
        return;
    }
    c_status = malloc(*count * sizeof(MPI_Status));
    if (c_status == NULL) {
        *ierr = MPI_ERR_INTERN;
        free(c_req);
        return;
    }

    for (i = 0; i < *count; i++) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = MPI_Waitall(*count, c_req, c_status);

    if (*ierr == MPI_SUCCESS) {
        for (i = 0; i < *count; i++) {
            if (c_req[i] == NULL)
              array_of_requests[i] = -1;
            MPI_Status_c2f( &c_status[i], &array_of_statuses[i]); 
        }
    }
    free(c_req);
    free(c_status);

}
