/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CLOSE_PORT = mpi_close_port_f
#pragma weak pmpi_close_port = mpi_close_port_f
#pragma weak pmpi_close_port_ = mpi_close_port_f
#pragma weak pmpi_close_port__ = mpi_close_port_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CLOSE_PORT,
                           pmpi_close_port,
                           pmpi_close_port_,
                           pmpi_close_port__,
                           pmpi_close_port_f,
                           (char *port_name, MPI_Fint *ierr),
                           (port_name, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CLOSE_PORT = mpi_close_port_f
#pragma weak mpi_close_port = mpi_close_port_f
#pragma weak mpi_close_port_ = mpi_close_port_f
#pragma weak mpi_close_port__ = mpi_close_port_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CLOSE_PORT,
                           mpi_close_port,
                           mpi_close_port_,
                           mpi_close_port__,
                           mpi_close_port_f,
                           (char *port_name, MPI_Fint *ierr),
                           (port_name, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_close_port_f(char *port_name, MPI_Fint *ierr)
{
    *ierr = MPI_Close_port(port_name);
}
