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
#pragma weak PMPI_CART_SUB = ompi_cart_sub_f
#pragma weak pmpi_cart_sub = ompi_cart_sub_f
#pragma weak pmpi_cart_sub_ = ompi_cart_sub_f
#pragma weak pmpi_cart_sub__ = ompi_cart_sub_f

#pragma weak PMPI_Cart_sub_f = ompi_cart_sub_f
#pragma weak PMPI_Cart_sub_f08 = ompi_cart_sub_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_SUB,
                           pmpi_cart_sub,
                           pmpi_cart_sub_,
                           pmpi_cart_sub__,
                           pompi_cart_sub_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *remain_dims, MPI_Fint *new_comm, MPI_Fint *ierr),
                           (comm, remain_dims, new_comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_SUB = ompi_cart_sub_f
#pragma weak mpi_cart_sub = ompi_cart_sub_f
#pragma weak mpi_cart_sub_ = ompi_cart_sub_f
#pragma weak mpi_cart_sub__ = ompi_cart_sub_f

#pragma weak MPI_Cart_sub_f = ompi_cart_sub_f
#pragma weak MPI_Cart_sub_f08 = ompi_cart_sub_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_CART_SUB,
                           mpi_cart_sub,
                           mpi_cart_sub_,
                           mpi_cart_sub__,
                           ompi_cart_sub_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *remain_dims, MPI_Fint *new_comm, MPI_Fint *ierr),
                           (comm, remain_dims, new_comm, ierr) )
#else
#define ompi_cart_sub_f pompi_cart_sub_f
#endif
#endif


void ompi_cart_sub_f(MPI_Fint *comm, ompi_fortran_logical_t *remain_dims,
                    MPI_Fint *new_comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm, c_new_comm;
    /*
     * Just in the case, when sizeof(logical)!=sizeof(int) and
     * Fortran TRUE-value != 1, we have to convert -- then we need
     * to know the number of dimensions, for the size of remain_dims
     */
#if OMPI_FORTRAN_MUST_CONVERT_LOGICAL_2_INT == 1
    int ndims;
#endif
    OMPI_LOGICAL_ARRAY_NAME_DECL(remain_dims);

    c_comm = PMPI_Comm_f2c(*comm);
    c_new_comm = PMPI_Comm_f2c(*new_comm);

#if OMPI_FORTRAN_MUST_CONVERT_LOGICAL_2_INT == 1
    *ierr = OMPI_INT_2_FINT(MPI_Cartdim_get(c_comm, &ndims));
    if (MPI_SUCCESS != OMPI_FINT_2_INT(*ierr)) {
        return;
    }
#endif
    OMPI_ARRAY_LOGICAL_2_INT(remain_dims, ndims);

    c_ierr = PMPI_Cart_sub(c_comm,
                          OMPI_LOGICAL_ARRAY_NAME_CONVERT(remain_dims),
                          &c_new_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *new_comm = PMPI_Comm_c2f(c_new_comm);
    }

    OMPI_ARRAY_INT_2_LOGICAL(remain_dims, ndims);
}
