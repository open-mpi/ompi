/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_SET = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set_ = mpi_errhandler_set_f
#pragma weak pmpi_errhandler_set__ = mpi_errhandler_set_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_SET,
                           pmpi_errhandler_set,
                           pmpi_errhandler_set_,
                           pmpi_errhandler_set__,
                           pmpi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_SET = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set_ = mpi_errhandler_set_f
#pragma weak mpi_errhandler_set__ = mpi_errhandler_set_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_SET,
                           mpi_errhandler_set,
                           mpi_errhandler_set_,
                           mpi_errhandler_set__,
                           mpi_errhandler_set_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_set_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
