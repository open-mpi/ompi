/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WTIME = mpi_wtime_f
#pragma weak pmpi_wtime = mpi_wtime_f
#pragma weak pmpi_wtime_ = mpi_wtime_f
#pragma weak pmpi_wtime__ = mpi_wtime_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WTIME,
                           pmpi_wtime,
                           pmpi_wtime_,
                           pmpi_wtime__,
                           pmpi_wtime_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTIME = mpi_wtime_f
#pragma weak mpi_wtime = mpi_wtime_f
#pragma weak mpi_wtime_ = mpi_wtime_f
#pragma weak mpi_wtime__ = mpi_wtime_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WTIME,
                           mpi_wtime,
                           mpi_wtime_,
                           mpi_wtime__,
                           mpi_wtime_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

double mpi_wtime_f(MPI_Fint *ierr)
{

}
