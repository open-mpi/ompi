/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
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
#pragma weak PMPI_CART_CREATE = ompi_cart_create_f
#pragma weak pmpi_cart_create = ompi_cart_create_f
#pragma weak pmpi_cart_create_ = ompi_cart_create_f
#pragma weak pmpi_cart_create__ = ompi_cart_create_f

#pragma weak PMPI_Cart_create_f = ompi_cart_create_f
#pragma weak PMPI_Cart_create_f08 = ompi_cart_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_CREATE,
                           pmpi_cart_create,
                           pmpi_cart_create_,
                           pmpi_cart_create__,
                           pompi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, ompi_fortran_logical_t *periods, ompi_fortran_logical_t *reorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, reorder, comm_cart, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_CREATE = ompi_cart_create_f
#pragma weak mpi_cart_create = ompi_cart_create_f
#pragma weak mpi_cart_create_ = ompi_cart_create_f
#pragma weak mpi_cart_create__ = ompi_cart_create_f

#pragma weak MPI_Cart_create_f = ompi_cart_create_f
#pragma weak MPI_Cart_create_f08 = ompi_cart_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_CART_CREATE,
                           mpi_cart_create,
                           mpi_cart_create_,
                           mpi_cart_create__,
                           ompi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, ompi_fortran_logical_t *periods, ompi_fortran_logical_t *reorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, reorder, comm_cart, ierr) )
#else
#define ompi_cart_create_f pompi_cart_create_f
#endif
#endif


void ompi_cart_create_f(MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims,
                       ompi_fortran_logical_t *periods, ompi_fortran_logical_t *reorder,
                       MPI_Fint *comm_cart, MPI_Fint *ierr)
{
    MPI_Comm c_comm1, c_comm2;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(dims);
    OMPI_LOGICAL_ARRAY_NAME_DECL(periods);

    c_comm1 = PMPI_Comm_f2c(*old_comm);

    size = OMPI_FINT_2_INT(*ndims);
    OMPI_ARRAY_FINT_2_INT(dims, size);
    OMPI_ARRAY_LOGICAL_2_INT(periods, size);

    c_ierr = PMPI_Cart_create(c_comm1, size,
                             OMPI_ARRAY_NAME_CONVERT(dims),
                             OMPI_LOGICAL_ARRAY_NAME_CONVERT(periods),
                             OMPI_LOGICAL_2_INT(*reorder),
                             &c_comm2);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *comm_cart = PMPI_Comm_c2f(c_comm2);
    }

    /*
     * Need to convert back into Fortran, to not surprise the user
     */
    OMPI_ARRAY_FINT_2_INT_CLEANUP(dims);
    OMPI_ARRAY_INT_2_LOGICAL(periods, size);
}
