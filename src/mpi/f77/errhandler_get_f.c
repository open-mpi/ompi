/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_GET = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get_ = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get__ = mpi_errhandler_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_GET,
                           pmpi_errhandler_get,
                           pmpi_errhandler_get_,
                           pmpi_errhandler_get__,
                           pmpi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_GET = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get_ = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get__ = mpi_errhandler_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_GET,
                           mpi_errhandler_get,
                           mpi_errhandler_get_,
                           mpi_errhandler_get__,
                           mpi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_get_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{

}
