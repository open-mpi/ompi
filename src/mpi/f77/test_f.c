/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TEST = mpi_test_f
#pragma weak pmpi_test = mpi_test_f
#pragma weak pmpi_test_ = mpi_test_f
#pragma weak pmpi_test__ = mpi_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST,
                           pmpi_test,
                           pmpi_test_,
                           pmpi_test__,
                           pmpi_test_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST = mpi_test_f
#pragma weak mpi_test = mpi_test_f
#pragma weak mpi_test_ = mpi_test_f
#pragma weak mpi_test__ = mpi_test_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TEST,
                           mpi_test,
                           mpi_test_,
                           mpi_test__,
                           mpi_test_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_test_f(MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Status c_status;

    *ierr = MPI_Test(&c_req, flag, &c_status);

    MPI_Status_c2f( &c_status, status); 

    if ( (MPI_SUCCESS == *ierr) && (NULL == c_req) ) {
      *request = -1;
    }
}
