/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_TEST_INTER = mpi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter = mpi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter_ = mpi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter__ = mpi_comm_test_inter_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_TEST_INTER,
                           pmpi_comm_test_inter,
                           pmpi_comm_test_inter_,
                           pmpi_comm_test_inter__,
                           pmpi_comm_test_inter_f,
                           (MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_TEST_INTER = mpi_comm_test_inter_f
#pragma weak mpi_comm_test_inter = mpi_comm_test_inter_f
#pragma weak mpi_comm_test_inter_ = mpi_comm_test_inter_f
#pragma weak mpi_comm_test_inter__ = mpi_comm_test_inter_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_TEST_INTER,
                           mpi_comm_test_inter,
                           mpi_comm_test_inter_,
                           mpi_comm_test_inter__,
                           mpi_comm_test_inter_f,
                           (MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, flag, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_test_inter_f(MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *ierr)
{

}
