/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TESTSOME = mpi_testsome_f
#pragma weak pmpi_testsome = mpi_testsome_f
#pragma weak pmpi_testsome_ = mpi_testsome_f
#pragma weak pmpi_testsome__ = mpi_testsome_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TESTSOME,
                           pmpi_testsome,
                           pmpi_testsome_,
                           pmpi_testsome__,
                           pmpi_testsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTSOME = mpi_testsome_f
#pragma weak mpi_testsome = mpi_testsome_f
#pragma weak mpi_testsome_ = mpi_testsome_f
#pragma weak mpi_testsome__ = mpi_testsome_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TESTSOME,
                           mpi_testsome,
                           mpi_testsome_,
                           mpi_testsome__,
                           mpi_testsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_testsome_f(MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i;

    c_req = malloc(*incount * sizeof(MPI_Request));
    if (c_req == NULL) {
        *ierr = MPI_ERR_INTERN;
        return;
    }

    c_status = malloc(*incount * sizeof(MPI_Status));
    if (c_status == NULL){
        *ierr = MPI_ERR_INTERN;
        free(c_req);
        return;
    }

    for (i = 0; i < *incount; i++) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = MPI_Testsome(*incount, c_req, outcount, array_of_indices, 
                         c_status);

    if (*ierr == MPI_SUCCESS) {
        if (*outcount != MPI_UNDEFINED) {
            for (i = 0; i < *outcount; i++) {
                array_of_indices[i] += 1;
            }
        }
        for (i = 0; i < *outcount; i++) {
          MPI_Status_c2f(&c_status[i], &array_of_statuses[i]);
        }
    }

    free(c_req);
    free(c_status);

}
