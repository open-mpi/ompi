/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ADDRESS = mpi_address_f
#pragma weak pmpi_address = mpi_address_f
#pragma weak pmpi_address_ = mpi_address_f
#pragma weak pmpi_address__ = mpi_address_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ADDRESS,
                           pmpi_address,
                           pmpi_address_,
                           pmpi_address__,
                           pmpi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADDRESS = mpi_address_f
#pragma weak mpi_address = mpi_address_f
#pragma weak mpi_address_ = mpi_address_f
#pragma weak mpi_address__ = mpi_address_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ADDRESS,
                           mpi_address,
                           mpi_address_,
                           mpi_address__,
                           mpi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_address_f(char *location, MPI_Fint *address, MPI_Fint *ierr)
{
    MPI_Aint addr;

    *ierr = OMPI_INT_2_FINT(MPI_Address(location, &addr));

    *address = (MPI_Fint) addr;
}
