/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_LOOKUP_NAME = mpi_lookup_name_f
#pragma weak pmpi_lookup_name = mpi_lookup_name_f
#pragma weak pmpi_lookup_name_ = mpi_lookup_name_f
#pragma weak pmpi_lookup_name__ = mpi_lookup_name_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_LOOKUP_NAME,
                           pmpi_lookup_name,
                           pmpi_lookup_name_,
                           pmpi_lookup_name__,
                           pmpi_lookup_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_LOOKUP_NAME = mpi_lookup_name_f
#pragma weak mpi_lookup_name = mpi_lookup_name_f
#pragma weak mpi_lookup_name_ = mpi_lookup_name_f
#pragma weak mpi_lookup_name__ = mpi_lookup_name_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_LOOKUP_NAME,
                           mpi_lookup_name,
                           mpi_lookup_name_,
                           mpi_lookup_name__,
                           mpi_lookup_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_lookup_name_f(char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr)
{

}
