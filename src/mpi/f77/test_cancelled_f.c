/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled__ = mpi_test_cancelled_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TEST_CANCELLED,
                           pmpi_test_cancelled,
                           pmpi_test_cancelled_,
                           pmpi_test_cancelled__,
                           pmpi_test_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled__ = mpi_test_cancelled_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TEST_CANCELLED,
                           mpi_test_cancelled,
                           mpi_test_cancelled_,
                           mpi_test_cancelled__,
                           mpi_test_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

void mpi_test_cancelled_f(MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr)
{

}
