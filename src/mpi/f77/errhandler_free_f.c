/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_FREE = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free_ = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free__ = mpi_errhandler_free_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_FREE,
                           pmpi_errhandler_free,
                           pmpi_errhandler_free_,
                           pmpi_errhandler_free__,
                           pmpi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_FREE = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free_ = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free__ = mpi_errhandler_free_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_FREE,
                           mpi_errhandler_free,
                           mpi_errhandler_free_,
                           mpi_errhandler_free__,
                           mpi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_errhandler_free_f(MPI_Fint *errhandler, MPI_Fint *ierr)
{

}
