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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GET_ADDRESS = ompi_get_address_f
#pragma weak pmpi_get_address = ompi_get_address_f
#pragma weak pmpi_get_address_ = ompi_get_address_f
#pragma weak pmpi_get_address__ = ompi_get_address_f

#pragma weak PMPI_Get_address_f = ompi_get_address_f
#pragma weak PMPI_Get_address_f08 = ompi_get_address_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_ADDRESS,
                           pmpi_get_address,
                           pmpi_get_address_,
                           pmpi_get_address__,
                           pompi_get_address_f,
                           (char *location, MPI_Aint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ADDRESS = ompi_get_address_f
#pragma weak mpi_get_address = ompi_get_address_f
#pragma weak mpi_get_address_ = ompi_get_address_f
#pragma weak mpi_get_address__ = ompi_get_address_f

#pragma weak MPI_Get_address_f = ompi_get_address_f
#pragma weak MPI_Get_address_f08 = ompi_get_address_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GET_ADDRESS,
                           mpi_get_address,
                           mpi_get_address_,
                           mpi_get_address__,
                           ompi_get_address_f,
                           (char *location, MPI_Aint *address, MPI_Fint *ierr),
                           (location, address, ierr) )
#else
#define ompi_get_address_f pompi_get_address_f
#endif
#endif


void ompi_get_address_f(char *location, MPI_Aint *address, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Aint c_address;

    c_ierr = PMPI_Get_address(OMPI_F2C_BOTTOM(location), &c_address);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *address = (MPI_Aint) c_address;
    }
}
