/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET_ADDRESS = ompi_get_address_f
#pragma weak pmpi_get_address = ompi_get_address_f
#pragma weak pmpi_get_address_ = ompi_get_address_f
#pragma weak pmpi_get_address__ = ompi_get_address_f

#pragma weak PMPI_Get_address_f = ompi_get_address_f
#pragma weak PMPI_Get_address_f08 = ompi_get_address_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_ADDRESS,
                           pmpi_get_address,
                           pmpi_get_address_,
                           pmpi_get_address__,
                           pompi_get_address_f,
                           (char *location, MPI_Aint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ADDRESS = ompi_get_address_f
#pragma weak mpi_get_address = ompi_get_address_f
#pragma weak mpi_get_address_ = ompi_get_address_f
#pragma weak mpi_get_address__ = ompi_get_address_f

#pragma weak MPI_Get_address_f = ompi_get_address_f
#pragma weak MPI_Get_address_f08 = ompi_get_address_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_ADDRESS,
                           mpi_get_address,
                           mpi_get_address_,
                           mpi_get_address__,
                           ompi_get_address_f,
                           (char *location, MPI_Aint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_get_address_f(char *location, MPI_Aint *address, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Aint c_address;

    c_ierr = MPI_Get_address(location, &c_address);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *address = (MPI_Aint) c_address;
    }
}
