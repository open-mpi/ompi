/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_FREE = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free_ = mpi_errhandler_free_f
#pragma weak pmpi_errhandler_free__ = mpi_errhandler_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_FREE,
                           pmpi_errhandler_free,
                           pmpi_errhandler_free_,
                           pmpi_errhandler_free__,
                           pmpi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_FREE = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free_ = mpi_errhandler_free_f
#pragma weak mpi_errhandler_free__ = mpi_errhandler_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_FREE,
                           mpi_errhandler_free,
                           mpi_errhandler_free_,
                           mpi_errhandler_free__,
                           mpi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_free_f(MPI_Fint *errhandler, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
