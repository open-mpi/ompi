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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ABI_SET_FOTRAN_BOOLEANS = ompi_abi_set_fortran_booleans_f
#pragma weak pmpi_abi_set_fortran_booleans = ompi_abi_set_fortran_booleans_f
#pragma weak pmpi_abi_set_fortran_booleans_ = ompi_abi_set_fortran_booleans_f
#pragma weak pmpi_abi_set_fortran_booleans__ = ompi_abi_set_fortran_booleans_f

#pragma weak PMPI_Abi_set_fortran_booleans_f = ompi_abi_set_fortran_booleans_f
#pragma weak PMPI_Abi_set_fortran_booleans_f08 = ompi_abi_set_fortran_booleans_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ABI_SET_FOTRAN_BOOLEANS,
                           pmpi_abi_set_fortran_booleans,
                           pmpi_abi_set_fortran_booleans_,
                           pmpi_abi_set_fortran_booleans__,
                           pompi_abi_set_fortran_booleans_f,
                           (MPI_Fint *logical_size, ompi_fortran_logical_t *logical_true,  ompi_fortran_logical_t *logical_false, MPI_Fint *ierr),
                           (logical_size, logical_true, logical_false, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ABI_SET_FOTRAN_BOOLEANS = ompi_abi_set_fortran_booleans_f
#pragma weak mpi_abi_set_fortran_booleans = ompi_abi_set_fortran_booleans_f
#pragma weak mpi_abi_set_fortran_booleans_ = ompi_abi_set_fortran_booleans_f
#pragma weak mpi_abi_set_fortran_booleans__ = ompi_abi_set_fortran_booleans_f

#pragma weak MPI_Abi_set_fortran_booleans_f = ompi_abi_set_fortran_booleans_f
#pragma weak MPI_Abi_set_fortran_booleans_f08 = ompi_abi_set_fortran_booleans_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ABI_SET_FOTRAN_BOOLEANS,
                           mpi_abi_set_fortran_booleans,
                           mpi_abi_set_fortran_booleans_,
                           mpi_abi_set_fortran_booleans__,
                           ompi_abi_set_fortran_booleans_f,
                           (MPI_Fint *logical_size, ompi_fortran_logical_t *logical_true,  ompi_fortran_logical_t *logical_false, MPI_Fint *ierr),
                           (logical_size, logical_true, logical_false, ierr) )
#else
#define ompi_abi_set_fortran_booleans_f pompi_abi_set_fortran_booleans_f
#endif
#endif

void ompi_abi_set_fortran_booleans_f(MPI_Fint *logical_size, 
                                     ompi_fortran_logical_t *logical_true, 
                                     ompi_fortran_logical_t *logical_false,
                                     MPI_Fint *ierr)
{
    int c_ierr, c_logical_true, c_logical_false;

    c_logical_true = OMPI_LOGICAL_2_INT(*logical_true);
    c_logical_false = OMPI_LOGICAL_2_INT(*logical_false);
    c_ierr = PMPI_Abi_set_fortran_booleans(OMPI_FINT_2_INT(*logical_size), 
                                           (void *)&c_logical_true,
                                           (void *)&c_logical_false);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
