/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TESTALL = mpi_testall_f
#pragma weak pmpi_testall = mpi_testall_f
#pragma weak pmpi_testall_ = mpi_testall_f
#pragma weak pmpi_testall__ = mpi_testall_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TESTALL,
                           pmpi_testall,
                           pmpi_testall_,
                           pmpi_testall__,
                           pmpi_testall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTALL = mpi_testall_f
#pragma weak mpi_testall = mpi_testall_f
#pragma weak mpi_testall_ = mpi_testall_f
#pragma weak mpi_testall__ = mpi_testall_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TESTALL,
                           mpi_testall,
                           mpi_testall_,
                           mpi_testall__,
                           mpi_testall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_testall_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{

}
