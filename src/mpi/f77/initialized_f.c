/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INITIALIZED = mpi_initialized_f
#pragma weak pmpi_initialized = mpi_initialized_f
#pragma weak pmpi_initialized_ = mpi_initialized_f
#pragma weak pmpi_initialized__ = mpi_initialized_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INITIALIZED,
                           pmpi_initialized,
                           pmpi_initialized_,
                           pmpi_initialized__,
                           pmpi_initialized_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INITIALIZED = mpi_initialized_f
#pragma weak mpi_initialized = mpi_initialized_f
#pragma weak mpi_initialized_ = mpi_initialized_f
#pragma weak mpi_initialized__ = mpi_initialized_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INITIALIZED,
                           mpi_initialized,
                           mpi_initialized_,
                           mpi_initialized__,
                           mpi_initialized_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

void mpi_initialized_f(MPI_Fint *flag, MPI_Fint *ierr)
{

}
