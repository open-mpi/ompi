/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_STATUS_SET_CANCELLED = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled_ = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled__ = mpi_status_set_cancelled_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_STATUS_SET_CANCELLED,
                           pmpi_status_set_cancelled,
                           pmpi_status_set_cancelled_,
                           pmpi_status_set_cancelled__,
                           pmpi_status_set_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_SET_CANCELLED = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled_ = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled__ = mpi_status_set_cancelled_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_STATUS_SET_CANCELLED,
                           mpi_status_set_cancelled,
                           mpi_status_set_cancelled_,
                           mpi_status_set_cancelled__,
                           mpi_status_set_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

void mpi_status_set_cancelled_f(MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr)
{

}
