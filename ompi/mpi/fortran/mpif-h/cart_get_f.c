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
#pragma weak PMPI_CART_GET = ompi_cart_get_f
#pragma weak pmpi_cart_get = ompi_cart_get_f
#pragma weak pmpi_cart_get_ = ompi_cart_get_f
#pragma weak pmpi_cart_get__ = ompi_cart_get_f

#pragma weak PMPI_Cart_get_f = ompi_cart_get_f
#pragma weak PMPI_Cart_get_f08 = ompi_cart_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_GET,
                           pmpi_cart_get,
                           pmpi_cart_get_,
                           pmpi_cart_get__,
                           pompi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, ompi_fortran_logical_t *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_GET = ompi_cart_get_f
#pragma weak mpi_cart_get = ompi_cart_get_f
#pragma weak mpi_cart_get_ = ompi_cart_get_f
#pragma weak mpi_cart_get__ = ompi_cart_get_f

#pragma weak MPI_Cart_get_f = ompi_cart_get_f
#pragma weak MPI_Cart_get_f08 = ompi_cart_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_GET,
                           mpi_cart_get,
                           mpi_cart_get_,
                           mpi_cart_get__,
                           ompi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, ompi_fortran_logical_t *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_cart_get_f(MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims,
		    ompi_fortran_logical_t *periods, MPI_Fint *coords, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(dims);
    OMPI_ARRAY_NAME_DECL(coords);
    OMPI_LOGICAL_ARRAY_NAME_DECL(periods);

    c_comm = MPI_Comm_f2c(*comm);

    size = OMPI_FINT_2_INT(*maxdims);
    OMPI_ARRAY_FINT_2_INT_ALLOC(dims, size);
    OMPI_ARRAY_FINT_2_INT_ALLOC(coords, size);
    OMPI_ARRAY_LOGICAL_2_INT_ALLOC(periods, size);

    c_ierr = MPI_Cart_get(c_comm,
                          size, 
                          OMPI_ARRAY_NAME_CONVERT(dims),
                          OMPI_LOGICAL_ARRAY_NAME_CONVERT(periods),
                          OMPI_ARRAY_NAME_CONVERT(coords));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_INT_2_FINT(dims, size);
    OMPI_ARRAY_INT_2_LOGICAL(periods, size);
    OMPI_ARRAY_INT_2_FINT(coords, size);
}
