/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WTICK = mpi_wtick_f
#pragma weak pmpi_wtick = mpi_wtick_f
#pragma weak pmpi_wtick_ = mpi_wtick_f
#pragma weak pmpi_wtick__ = mpi_wtick_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WTICK,
                           pmpi_wtick,
                           pmpi_wtick_,
                           pmpi_wtick__,
                           pmpi_wtick_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTICK = mpi_wtick_f
#pragma weak mpi_wtick = mpi_wtick_f
#pragma weak mpi_wtick_ = mpi_wtick_f
#pragma weak mpi_wtick__ = mpi_wtick_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WTICK,
                           mpi_wtick,
                           mpi_wtick_,
                           mpi_wtick__,
                           mpi_wtick_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

double mpi_wtick_f(MPI_Fint *ierr)
{

}
