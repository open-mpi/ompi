/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_START = mpi_start_f
#pragma weak pmpi_start = mpi_start_f
#pragma weak pmpi_start_ = mpi_start_f
#pragma weak pmpi_start__ = mpi_start_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_START,
                           pmpi_start,
                           pmpi_start_,
                           pmpi_start__,
                           pmpi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_START = mpi_start_f
#pragma weak mpi_start = mpi_start_f
#pragma weak mpi_start_ = mpi_start_f
#pragma weak mpi_start__ = mpi_start_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_START,
                           mpi_start,
                           mpi_start_,
                           mpi_start__,
                           mpi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_start_f(MPI_Fint *request, MPI_Fint *ierr)
{

}
