/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TEST = mpi_test_f
#pragma weak pmpi_test = mpi_test_f
#pragma weak pmpi_test_ = mpi_test_f
#pragma weak pmpi_test__ = mpi_test_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TEST,
                           pmpi_test,
                           pmpi_test_,
                           pmpi_test__,
                           pmpi_test_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST = mpi_test_f
#pragma weak mpi_test = mpi_test_f
#pragma weak mpi_test_ = mpi_test_f
#pragma weak mpi_test__ = mpi_test_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TEST,
                           mpi_test,
                           mpi_test_,
                           mpi_test__,
                           mpi_test_f,
                           (MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_test_f(MPI_Fint *request, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{

}
