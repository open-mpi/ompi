/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CANCEL = mpi_cancel_f
#pragma weak pmpi_cancel = mpi_cancel_f
#pragma weak pmpi_cancel_ = mpi_cancel_f
#pragma weak pmpi_cancel__ = mpi_cancel_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CANCEL,
                           pmpi_cancel,
                           pmpi_cancel_,
                           pmpi_cancel__,
                           pmpi_cancel_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CANCEL = mpi_cancel_f
#pragma weak mpi_cancel = mpi_cancel_f
#pragma weak mpi_cancel_ = mpi_cancel_f
#pragma weak mpi_cancel__ = mpi_cancel_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CANCEL,
                           mpi_cancel,
                           mpi_cancel_,
                           mpi_cancel__,
                           mpi_cancel_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_cancel_f(MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);

    *ierr = OMPI_INT_2_FINT(MPI_Cancel(&c_req));
}
