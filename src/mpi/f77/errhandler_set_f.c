/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_SET = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set_ = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set__ = mpi_errhandler_set_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_SET,
                           pmpi_errhandler_set,
                           pmpi_errhandler_set_,
                           pmpi_errhandler_set__,
                           pmpi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_SET = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set_ = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set__ = mpi_errhandler_set_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_SET,
                           mpi_errhandler_set,
                           mpi_errhandler_set_,
                           mpi_errhandler_set__,
                           mpi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

void mpi_errhandler_set_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{

}
