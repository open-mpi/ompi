/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_JOIN = mpi_comm_join_f
#pragma weak pmpi_comm_join = mpi_comm_join_f
#pragma weak pmpi_comm_join_ = mpi_comm_join_f
#pragma weak pmpi_comm_join__ = mpi_comm_join_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_JOIN,
                           pmpi_comm_join,
                           pmpi_comm_join_,
                           pmpi_comm_join__,
                           pmpi_comm_join_f,
                           (MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr),
                           (fd, intercomm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_JOIN = mpi_comm_join_f
#pragma weak mpi_comm_join = mpi_comm_join_f
#pragma weak mpi_comm_join_ = mpi_comm_join_f
#pragma weak mpi_comm_join__ = mpi_comm_join_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_JOIN,
                           mpi_comm_join,
                           mpi_comm_join_,
                           mpi_comm_join__,
                           mpi_comm_join_f,
                           (MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr),
                           (fd, intercomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_join_f(MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr)
{
    MPI_Comm c_intercomm;

    *ierr = OMPI_INT_2_FINT(MPI_Comm_join(OMPI_FINT_2_INT(*fd),
					  &c_intercomm));
    *intercomm = MPI_Comm_c2f(c_intercomm);
}
