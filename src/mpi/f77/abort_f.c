/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ABORT = mpi_abort_f
#pragma weak pmpi_abort = mpi_abort_f
#pragma weak pmpi_abort_ = mpi_abort_f
#pragma weak pmpi_abort__ = mpi_abort_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ABORT,
                           pmpi_abort,
                           pmpi_abort_,
                           pmpi_abort__,
                           pmpi_abort_f,
                           (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (comm, errorcode, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ABORT = mpi_abort_f
#pragma weak mpi_abort = mpi_abort_f
#pragma weak mpi_abort_ = mpi_abort_f
#pragma weak mpi_abort__ = mpi_abort_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ABORT,
                           mpi_abort,
                           mpi_abort_,
                           mpi_abort__,
                           mpi_abort_f,
                           (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (comm, errorcode, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_abort_f(MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr)
{

}
