/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ADDRESS = mpi_address_f
#pragma weak pmpi_address = mpi_address_f
#pragma weak pmpi_address_ = mpi_address_f
#pragma weak pmpi_address__ = mpi_address_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ADDRESS,
                           pmpi_address,
                           pmpi_address_,
                           pmpi_address__,
                           pmpi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADDRESS = mpi_address_f
#pragma weak mpi_address = mpi_address_f
#pragma weak mpi_address_ = mpi_address_f
#pragma weak mpi_address__ = mpi_address_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ADDRESS,
                           mpi_address,
                           mpi_address_,
                           mpi_address__,
                           mpi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_address_f(char *location, MPI_Fint *address, MPI_Fint *ierr)
{

}
