/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_CREATE = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create_ = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create__ = mpi_errhandler_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_CREATE,
                           pmpi_errhandler_create,
                           pmpi_errhandler_create_,
                           pmpi_errhandler_create__,
                           pmpi_errhandler_create_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_CREATE = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create_ = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create__ = mpi_errhandler_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_CREATE,
                           mpi_errhandler_create,
                           mpi_errhandler_create_,
                           mpi_errhandler_create__,
                           mpi_errhandler_create_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

void mpi_errhandler_create_f(MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr)
{

}
