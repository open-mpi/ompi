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
#pragma weak PMPI_CART_COORDS = ompi_cart_coords_f
#pragma weak pmpi_cart_coords = ompi_cart_coords_f
#pragma weak pmpi_cart_coords_ = ompi_cart_coords_f
#pragma weak pmpi_cart_coords__ = ompi_cart_coords_f

#pragma weak PMPI_Cart_coords_f = ompi_cart_coords_f
#pragma weak PMPI_Cart_coords_f08 = ompi_cart_coords_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_COORDS,
                           pmpi_cart_coords,
                           pmpi_cart_coords_,
                           pmpi_cart_coords__,
                           pompi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_COORDS = ompi_cart_coords_f
#pragma weak mpi_cart_coords = ompi_cart_coords_f
#pragma weak mpi_cart_coords_ = ompi_cart_coords_f
#pragma weak mpi_cart_coords__ = ompi_cart_coords_f

#pragma weak MPI_Cart_coords_f = ompi_cart_coords_f
#pragma weak MPI_Cart_coords_f08 = ompi_cart_coords_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_CART_COORDS,
                           mpi_cart_coords,
                           mpi_cart_coords_,
                           mpi_cart_coords__,
                           ompi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#else
#define ompi_cart_coords_f pompi_cart_coords_f
#endif
#endif


void ompi_cart_coords_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims,
                       MPI_Fint *coords, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(coords);

    c_comm = PMPI_Comm_f2c(*comm);

    OMPI_ARRAY_FINT_2_INT_ALLOC(coords, OMPI_FINT_2_INT(*maxdims));
    c_ierr = PMPI_Cart_coords(c_comm,
                             OMPI_FINT_2_INT(*rank),
                             OMPI_FINT_2_INT(*maxdims),
                             OMPI_ARRAY_NAME_CONVERT(coords));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_ARRAY_INT_2_FINT(coords, OMPI_FINT_2_INT(*maxdims));
    } else {
        OMPI_ARRAY_FINT_2_INT_CLEANUP(coords);
    }
}
