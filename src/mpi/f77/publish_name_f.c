/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_PUBLISH_NAME = mpi_publish_name_f
#pragma weak pmpi_publish_name = mpi_publish_name_f
#pragma weak pmpi_publish_name_ = mpi_publish_name_f
#pragma weak pmpi_publish_name__ = mpi_publish_name_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_PUBLISH_NAME,
                           pmpi_publish_name,
                           pmpi_publish_name_,
                           pmpi_publish_name__,
                           pmpi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PUBLISH_NAME = mpi_publish_name_f
#pragma weak mpi_publish_name = mpi_publish_name_f
#pragma weak mpi_publish_name_ = mpi_publish_name_f
#pragma weak mpi_publish_name__ = mpi_publish_name_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_PUBLISH_NAME,
                           mpi_publish_name,
                           mpi_publish_name_,
                           mpi_publish_name__,
                           mpi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif

void mpi_publish_name_f(char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr)
{

}
