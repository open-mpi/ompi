/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TESTANY = mpi_testany_f
#pragma weak pmpi_testany = mpi_testany_f
#pragma weak pmpi_testany_ = mpi_testany_f
#pragma weak pmpi_testany__ = mpi_testany_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TESTANY,
                           pmpi_testany,
                           pmpi_testany_,
                           pmpi_testany__,
                           pmpi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, flag, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTANY = mpi_testany_f
#pragma weak mpi_testany = mpi_testany_f
#pragma weak mpi_testany_ = mpi_testany_f
#pragma weak mpi_testany__ = mpi_testany_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TESTANY,
                           mpi_testany,
                           mpi_testany_,
                           mpi_testany__,
                           mpi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, flag, status, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_testany_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{

}
