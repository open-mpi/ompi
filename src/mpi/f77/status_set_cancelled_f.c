/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_STATUS_SET_CANCELLED = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled_ = mpi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled__ = mpi_status_set_cancelled_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_STATUS_SET_CANCELLED,
                           pmpi_status_set_cancelled,
                           pmpi_status_set_cancelled_,
                           pmpi_status_set_cancelled__,
                           pmpi_status_set_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_SET_CANCELLED = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled_ = mpi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled__ = mpi_status_set_cancelled_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_STATUS_SET_CANCELLED,
                           mpi_status_set_cancelled,
                           mpi_status_set_cancelled_,
                           mpi_status_set_cancelled__,
                           mpi_status_set_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_status_set_cancelled_f(MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr)
{

    MPI_Status c_status;

    MPI_Status_f2c( status, &c_status );

    *ierr = MPI_Status_set_cancelled(&c_status, *flag);

    if (*ierr == MPI_SUCCESS)
        MPI_Status_c2f(&c_status, status);
}
