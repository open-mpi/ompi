/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_CONNECT = mpi_comm_connect_f
#pragma weak pmpi_comm_connect = mpi_comm_connect_f
#pragma weak pmpi_comm_connect_ = mpi_comm_connect_f
#pragma weak pmpi_comm_connect__ = mpi_comm_connect_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CONNECT,
                           pmpi_comm_connect,
                           pmpi_comm_connect_,
                           pmpi_comm_connect__,
                           pmpi_comm_connect_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (port_name, info, root, comm, newcomm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CONNECT = mpi_comm_connect_f
#pragma weak mpi_comm_connect = mpi_comm_connect_f
#pragma weak mpi_comm_connect_ = mpi_comm_connect_f
#pragma weak mpi_comm_connect__ = mpi_comm_connect_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CONNECT,
                           mpi_comm_connect,
                           mpi_comm_connect_,
                           mpi_comm_connect__,
                           mpi_comm_connect_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (port_name, info, root, comm, newcomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_connect_f(char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr)
{

}
