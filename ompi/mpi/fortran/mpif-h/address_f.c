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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ADDRESS = ompi_address_f
#pragma weak pmpi_address = ompi_address_f
#pragma weak pmpi_address_ = ompi_address_f
#pragma weak pmpi_address__ = ompi_address_f

#pragma weak PMPI_Address_f = ompi_address_f
#pragma weak PMPI_Address_f08 = ompi_address_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ADDRESS,
                           pmpi_address,
                           pmpi_address_,
                           pmpi_address__,
                           pompi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADDRESS = ompi_address_f
#pragma weak mpi_address = ompi_address_f
#pragma weak mpi_address_ = ompi_address_f
#pragma weak mpi_address__ = ompi_address_f

#pragma weak MPI_Address_f = ompi_address_f
#pragma weak MPI_Address_f08 = ompi_address_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ADDRESS,
                           mpi_address,
                           mpi_address_,
                           mpi_address__,
                           ompi_address_f,
                           (char *location, MPI_Fint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#else
#define ompi_address_f pompi_address_f
#endif
#endif


void ompi_address_f(char *location, MPI_Fint *address, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Aint addr;

    ierr_c = PMPI_Address(location, &addr);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) {
        *address = (MPI_Fint) addr;
    }
}
