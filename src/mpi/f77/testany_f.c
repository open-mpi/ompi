/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TESTANY = mpi_testany_f
#pragma weak pmpi_testany = mpi_testany_f
#pragma weak pmpi_testany_ = mpi_testany_f
#pragma weak pmpi_testany__ = mpi_testany_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TESTANY,
                           pmpi_testany,
                           pmpi_testany_,
                           pmpi_testany__,
                           pmpi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, flag, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTANY = mpi_testany_f
#pragma weak mpi_testany = mpi_testany_f
#pragma weak mpi_testany_ = mpi_testany_f
#pragma weak mpi_testany__ = mpi_testany_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TESTANY,
                           mpi_testany,
                           mpi_testany_,
                           mpi_testany__,
                           mpi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_testany_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i;

    c_req = malloc(*count * sizeof(MPI_Request));
    if (c_req == NULL) {
        *ierr = MPI_ERR_INTERN;
        return;
    }

    for (i = 0; i < *count; i++) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = MPI_Testany(*count, c_req, index, flag, &c_status);

    if (*ierr == MPI_SUCCESS) {
        if (*index != MPI_UNDEFINED) {
            *index += 1;
        }
        MPI_Status_c2f(&c_status, status); 
    }

    free(c_req);
}
