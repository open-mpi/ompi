/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GET_ADDRESS = mpi_get_address_f
#pragma weak pmpi_get_address = mpi_get_address_f
#pragma weak pmpi_get_address_ = mpi_get_address_f
#pragma weak pmpi_get_address__ = mpi_get_address_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GET_ADDRESS,
                           pmpi_get_address,
                           pmpi_get_address_,
                           pmpi_get_address__,
                           pmpi_get_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ADDRESS = mpi_get_address_f
#pragma weak mpi_get_address = mpi_get_address_f
#pragma weak mpi_get_address_ = mpi_get_address_f
#pragma weak mpi_get_address__ = mpi_get_address_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GET_ADDRESS,
                           mpi_get_address,
                           mpi_get_address_,
                           mpi_get_address__,
                           mpi_get_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_get_address_f(char *location, MPI_Fint *address, MPI_Fint *ierr)
{

}
