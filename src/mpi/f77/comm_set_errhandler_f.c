/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SET_ERRHANDLER = mpi_comm_set_errhandler_f
#pragma weak pmpi_comm_set_errhandler = mpi_comm_set_errhandler_f
#pragma weak pmpi_comm_set_errhandler_ = mpi_comm_set_errhandler_f
#pragma weak pmpi_comm_set_errhandler__ = mpi_comm_set_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SET_ERRHANDLER,
                           pmpi_comm_set_errhandler,
                           pmpi_comm_set_errhandler_,
                           pmpi_comm_set_errhandler__,
                           pmpi_comm_set_errhandler_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_ERRHANDLER = mpi_comm_set_errhandler_f
#pragma weak mpi_comm_set_errhandler = mpi_comm_set_errhandler_f
#pragma weak mpi_comm_set_errhandler_ = mpi_comm_set_errhandler_f
#pragma weak mpi_comm_set_errhandler__ = mpi_comm_set_errhandler_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SET_ERRHANDLER,
                           mpi_comm_set_errhandler,
                           mpi_comm_set_errhandler_,
                           mpi_comm_set_errhandler__,
                           mpi_comm_set_errhandler_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_set_errhandler_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
