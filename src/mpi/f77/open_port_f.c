/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_OPEN_PORT = mpi_open_port_f
#pragma weak pmpi_open_port = mpi_open_port_f
#pragma weak pmpi_open_port_ = mpi_open_port_f
#pragma weak pmpi_open_port__ = mpi_open_port_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_OPEN_PORT,
                           pmpi_open_port,
                           pmpi_open_port_,
                           pmpi_open_port__,
                           pmpi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (info, port_name, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OPEN_PORT = mpi_open_port_f
#pragma weak mpi_open_port = mpi_open_port_f
#pragma weak mpi_open_port_ = mpi_open_port_f
#pragma weak mpi_open_port__ = mpi_open_port_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_OPEN_PORT,
                           mpi_open_port,
                           mpi_open_port_,
                           mpi_open_port__,
                           mpi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (info, port_name, ierr) )
#endif

void mpi_open_port_f(MPI_Fint *info, char *port_name, MPI_Fint *ierr)
{

}
